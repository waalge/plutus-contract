--FIXME can go module the reexported types that don't go.
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
module Plutus.Contract(
      Contract(..)
    , Plutus.Contract.Types.ContractError(..)
    , Plutus.Contract.Types.AsContractError(..)
    , Plutus.Contract.Types.IsContract(..)
    , (>>)
    , Plutus.Contract.Types.throwError
    , Plutus.Contract.Types.handleError
    , Plutus.Contract.Types.mapError
    , Plutus.Contract.Types.runError
    -- * Select
    , Promise
    , Plutus.Contract.Types.awaitPromise
    , Plutus.Contract.Types.promiseMap
    , Plutus.Contract.Types.promiseBind
    , both
    , Plutus.Contract.Types.selectEither
    , Plutus.Contract.Types.select
    , Plutus.Contract.Types.selectList
    , Plutus.Contract.Types.never
    -- * Endpoints
    , Request.HasEndpoint
    , Request.EndpointDescription(..)
    , Request.Endpoint
    , Schema.EmptySchema
    -- * Wallet's information
    -- * Contract instance Id
    , Wallet.Types.ContractInstanceId
    -- * Notifications
    , tell
    -- * Transactions
    , WalletAPIError
    , Request.adjustUnbalancedTx
    , Request.balanceTx
    -- * Checkpoints
    , Plutus.Contract.Types.AsCheckpointError(..)
    , Plutus.Contract.Types.CheckpointError(..)
    -- * Logging
    , module Logging
    -- * Row-related things
    , HasType
    , ContractRow
    , type (.\/)
    , type Empty
    ) where

import Data.Row (Empty, HasType, type (.\/))

import Plutus.Contract.Logging as Logging
import Plutus.Contract.Request (ContractRow)
import Plutus.Contract.Request qualified as Request
import Plutus.Contract.Schema qualified as Schema
import Plutus.Contract.Types (Contract (Contract), Promise, select)
import Plutus.Contract.Types qualified

import Control.Monad.Freer.Writer qualified as W
import Data.Functor.Apply (liftF2)
import Wallet.API (WalletAPIError)
import Wallet.Types qualified

-- | Execute both contracts in any order
both :: Promise w s e a -> Promise w s e b -> Promise w s e (a, b)
both a b = liftF2 (,) a b `select` liftF2 (flip (,)) b a

-- | Update the contract's accumulating state @w@
tell :: w -> Contract w s e ()
tell = Contract . W.tell
