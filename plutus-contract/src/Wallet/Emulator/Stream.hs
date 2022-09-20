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
    , params
    , initialChainState
    ) where

import Control.Lens (makeLenses)
import Data.Default (Default (def))
import Ledger.Tx (CardanoTx (..))
import Plutus.ChainIndex (ChainIndexError)
import Wallet.API (WalletAPIError)
import Wallet.Emulator qualified as EM

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
