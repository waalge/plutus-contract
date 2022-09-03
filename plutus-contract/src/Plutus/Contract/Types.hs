{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
module Plutus.Contract.Types(
    ContractEffs
    , Contract(..)
    , IsContract(..)
    -- * Select
    , Promise(..)
    , promiseBind
    , promiseMap
    , select
    , selectEither
    , selectList
    , never
    -- * Error handling
    , Plutus.Contract.Error.ContractError(..)
    , Plutus.Contract.Error.AsContractError(..)
    , Plutus.Contract.Error.MatchingError(..)
    , mapError
    , runError
    , handleError
    , throwError
    -- * Checkpoints
    , AsCheckpointError(..)
    , CheckpointError(..)
    -- * State
    , ResumableResult(..)
    -- * Run with continuations
    , SuspendedContract(..)
    ) where

import Control.Lens (Bifunctor (bimap), Iso', iso, makeLenses, over, set, to, unto, view, (&), (.~), (^.))
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, run, send, subsume, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Error qualified as E
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, handleLogIgnore, handleLogWriter)
import Control.Monad.Freer.Extras.Modify (raiseEnd, raiseUnder, writeIntoState)
import Control.Monad.Freer.State (State, get, put, runState)
import Control.Monad.Freer.Writer (Writer)
import Control.Monad.Freer.Writer qualified as W
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor.Apply (Apply, liftF2)
import Data.IntervalSet qualified as IS
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Row (Row)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

import Plutus.Contract.Checkpoint (AsCheckpointError (_CheckpointError),
                                   Checkpoint (AllocateKey, DoCheckpoint, Retrieve, Store),
                                   CheckpointError (JSONDecodeError), CheckpointKey, CheckpointLogMsg, CheckpointStore,
                                   completedIntervals, handleCheckpoint, jsonCheckpoint, jsonCheckpointLoop)
import Plutus.Contract.Effects (PABReq, PABResp)
import Plutus.Contract.Error qualified
import Plutus.Contract.Resumable (IterationID, MultiRequestContStatus (AContinuation, AResult),
                                  MultiRequestContinuation (MultiRequestContinuation, ndcCont, ndcRequests), RequestID,
                                  Requests, Response, Responses, Resumable, _Responses, handleResumable, insertResponse,
                                  suspendNonDet)
import Plutus.Contract.Resumable qualified as Resumable

import Prelude as Haskell

-- | Effects that are available to contracts.
type ContractEffs w e =
    '[ Error e
    ,  LogMsg Value
    ,  Writer w
    ,  Checkpoint
    ,  Resumable PABResp PABReq
    ]

type ContractEnv = (IterationID, RequestID)

newtype AccumState w = AccumState { unAccumState :: w }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid, Aeson.ToJSON, Aeson.FromJSON)

_AccumState :: forall w. Iso' (AccumState w) w
_AccumState = iso unAccumState AccumState

-- | @Contract w s e a@ is a contract with schema 's', producing a value of
--  type 'a' or an error 'e'. See note [Contract Schema].
--
newtype Contract w (s :: Row *) e a = Contract { unContract :: Eff (ContractEffs w e) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadError e (Contract w s e) where
    throwError = Contract . E.throwError
    catchError (Contract f) handler =
      Contract
      $ E.catchError f
      $ unContract . handler

instance Bifunctor (Contract w s) where
  bimap l r = mapError l . fmap r

-- | Transform any exceptions thrown by the 'Contract' using the given function.
mapError ::
  forall w s e e' a.
  (e -> e')
  -> Contract w s e a
  -> Contract w s e' a
mapError f = handleError (throwError . f)

-- | Turn a contract with error type 'e' and return type 'a' into one with
--   any error type (ie. throwing no errors) that returns 'Either e a'
runError ::
  forall w s e e0 a.
  Contract w s e a
  -> Contract w s e0 (Either e a)
runError (Contract r) = Contract (E.runError $ raiseUnder r)

-- | Handle errors, potentially throwing new errors.
handleError ::
  forall w s e e' a.
  (e -> Contract w s e' a)
  -> Contract w s e a
  -> Contract w s e' a
handleError f (Contract c) = Contract c' where
  c' = E.handleError @e (raiseUnder c) (fmap unContract f)

instance Semigroup a => Semigroup (Contract w s e a) where
  Contract ma <> Contract ma' = Contract $ (<>) <$> ma <*> ma'

-- | A wrapper indicating that this contract starts with a waiting action. For use with @select@.
newtype Promise w (s :: Row *) e a = Promise { awaitPromise :: Contract w s e a }
  deriving newtype (Functor, Bifunctor, Semigroup)

instance Apply (Promise w s e) where
  liftF2 f (Promise a) (Promise b) = Promise (f <$> a <*> b)

-- | Class of types that can be trivially converted to a `Contract`.
-- For use with functions where it is convenient to accept both `Contract` and `Promise` types.
class IsContract c where
  toContract :: c w s e a -> Contract w s e a

instance IsContract Contract where
  toContract = id

instance IsContract Promise where
  toContract = awaitPromise


-- | @select@ returns the contract that makes progress first, discarding the
--   other one.
--
-- However, note that if multiples promises are chained together like
-- @P1 `select` P2 `select` P3@ and all three can make progress at the same
-- moment, then @select@ will prioritize the promises starting from the right
-- (first @P3@ then @P2@ then @P1@).
select :: forall w s e a. Promise w s e a -> Promise w s e a -> Promise w s e a
select (Promise (Contract l)) (Promise (Contract r)) = Promise (Contract (Resumable.select @PABResp @PABReq @(ContractEffs w e) l r))

-- | A variant of @select@ for contracts with different return types.
selectEither :: forall w s e a b. Promise w s e a -> Promise w s e b -> Promise w s e (Either a b)
selectEither l r = (Left <$> l) `select` (Right <$> r)

-- | A `Promise` that is never fulfilled. This is the identity of `select`.
never :: Promise w s e a
never = Promise (Contract $ Resumable.never @PABResp @PABReq)

-- | Run more `Contract` code after the `Promise`.
promiseBind :: Promise w s e a -> (a -> Contract w s e b) -> Promise w s e b
promiseBind (Promise ma) f = Promise (ma >>= f)

-- | Lift a mapping function for `Contract` to a mapping function for `Promise`.
promiseMap :: (Contract w1 s1 e1 a1 -> Contract w2 s2 e2 a2) -> Promise w1 s1 e1 a1 -> Promise w2 s2 e2 a2
promiseMap f (Promise ma) = Promise (f ma)

-- | 'selectList' returns the contract that makes progress first, discarding the
-- other ones.
--
-- However, if multiple contracts can make progress, 'selectList' prioritizes
-- the ones appearing first in the input list. Therefore, the order of the
-- list of promises is important.
selectList :: [Promise w s e a] -> Contract w s e a
selectList = awaitPromise . foldr1 select . reverse

type SuspendedContractEffects w e =
  Error e
  ': State CheckpointKey
  ': State CheckpointStore
  ': LogMsg CheckpointLogMsg
  ': State (AccumState w)
  ': LogMsg Value
  ': Writer (Seq (LogMessage Value))
  ': '[]

-- | The result of running a 'Resumable'
data ResumableResult w e i o a =
    ResumableResult
        { _responses       :: Responses (CheckpointKey, i) -- The record with the resumable's execution history
        , _requests        :: Requests o -- Handlers that the 'Resumable' has registered
        , _finalState      :: Either e (Maybe a) -- Error or final state of the 'Resumable' (if it has finished)
        , _logs            :: Seq (LogMessage Value) -- All log messages that have been produced by this instance.
        , _lastLogs        :: Seq (LogMessage Value) -- Log messages produced in the last step
        , _checkpointStore :: CheckpointStore
        , _observableState :: w -- ^ Accumulated, observable state of the contract
        , _lastState       :: w -- ^ Last accumulated state
        }
        deriving stock (Generic, Show)
        deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

makeLenses ''ResumableResult

data SuspendedContract w e i o a =
  SuspendedContract
    { _resumableResult :: ResumableResult w e i o a
    , _continuations   :: Maybe (MultiRequestContStatus i o (SuspendedContractEffects w e) a)
    , _checkpointKey   :: CheckpointKey
    }

makeLenses ''SuspendedContract
