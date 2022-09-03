--FIXME: can go completely
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- | Running emulator actions that produce streams of events
module Wallet.Emulator.Stream(
    -- * Emulator streams
    EmulatorConfig(..)
    , EmulatorErr(..)
    , InitialChainState
    ) where

import Control.Foldl qualified as L
import Control.Lens (filtered, makeLenses, preview, view)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, run, subsume, type (~>))
import Control.Monad.Freer.Coroutine (Yield, yield)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras (raiseEnd, wrapError)
import Control.Monad.Freer.Extras.Log (LogLevel, LogMessage (LogMessage, _logLevel), LogMsg (LMessage),
                                       logMessageContent, mapMLog)
import Control.Monad.Freer.Extras.Stream (runStream)
import Control.Monad.Freer.State (State, gets, runState)
import Data.Bifunctor (first)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Ledger.AddressMap qualified as AM
import Ledger.Blockchain (Block, OnChainTx (Valid))
import Ledger.Slot (Slot)
import Ledger.Tx (CardanoTx (..))
import Ledger.Value (Value)
import Plutus.ChainIndex (ChainIndexError)
import Streaming (Stream)
import Streaming qualified as S
import Streaming.Prelude (Of)
import Streaming.Prelude qualified as S
import Wallet.API (WalletAPIError)
import Wallet.Emulator (EmulatorEvent, EmulatorEvent')
import Wallet.Emulator qualified as EM
import Wallet.Emulator.Chain (ChainControlEffect, ChainEffect, _SlotAdd)
import Wallet.Emulator.MultiAgent (EmulatorState, EmulatorTimeEvent (EmulatorTimeEvent), MultiAgentControlEffect,
                                   MultiAgentEffect, chainEvent, eteEvent)
import Wallet.Emulator.Wallet (Wallet, mockWalletAddress)

import Ledger.Params (Params)
import Plutus.Contract.Trace (InitialDistribution, defaultDist)
import Plutus.Trace.Emulator.Types (EmulatorRuntimeError)

{- Note [Emulator event stream]

The primary way of observing the outcome of a trace is by looking at the
stream of events it produces, via 'runTraceStream'. This has the following
reasons:

* A totally ordered stream of events is a good way to characterise the
  behaviour of a dynamic system.
* By taking the stream of events as the main output of running a trace, we
  can potentially run the trace against a live system. (To really do that we'll have to change the type of log messages - 'EmulatorEvent' contains some events only make sense in the emulator. But the underlying mechanism of how the stream is produces is still the same.) See note [The Emulator Control effect]
* We have the potential of saving some work because the stream is produced
  on-demand. This also makes it possible to deal with infinite traces: We just
  evaluate them to a finite number of steps.

-}

data EmulatorConfig =
    EmulatorConfig
        { _initialChainState :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        , _params            :: Params -- ^ Set the protocol parameters, network ID and slot configuration for the emulator.
        } deriving (Eq, Show)

type InitialChainState = Either InitialDistribution [CardanoTx]

instance Default EmulatorConfig where
  def = EmulatorConfig
          { _initialChainState = Left defaultDist
          , _params = def
          }

data EmulatorErr =
    WalletErr WalletAPIError
    | ChainIndexErr ChainIndexError
    | AssertionErr EM.AssertionError
    | InstanceErr EmulatorRuntimeError
    deriving (Show)

makeLenses ''EmulatorConfig
