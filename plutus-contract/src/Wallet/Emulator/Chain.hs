{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Wallet.Emulator.Chain where

import Control.Applicative ((<|>))
import Control.Lens hiding (index)
import Control.Monad.Freer
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import Control.Monad.Freer.State
import Control.Monad.State qualified as S
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List (partition, (\\))
import Data.Maybe (mapMaybe)
import Data.Monoid (Ap (Ap))
import Data.Traversable (for)
import GHC.Generics (Generic)
import Ledger (Block, Blockchain, CardanoTx (..), EmulatorEra, OnChainTx (..), Params (..), ScriptValidationEvent,
               Slot (..), SomeCardanoApiTx (CardanoApiEmulatorEraTx), TxId, TxIn (txInRef), TxOut (txOutValue), Value,
               eitherTx, getCardanoTxCollateralInputs, getCardanoTxFee, getCardanoTxId, getCardanoTxValidityRange,
               mergeCardanoTxWith)
import Ledger.Index qualified as Index
import Ledger.Interval qualified as Interval
import Ledger.Validation qualified as Validation
import Plutus.Contract.Util (uncurry3)
import Prettyprinter

-- | Events produced by the blockchain emulator.
data ChainEvent =
    TxnValidate TxId CardanoTx [ScriptValidationEvent]
    -- ^ A transaction has been validated and added to the blockchain.
    | TxnValidationFail Index.ValidationPhase TxId CardanoTx Index.ValidationError [ScriptValidationEvent] Value
    -- ^ A transaction failed to validate. The @Value@ indicates the amount of collateral stored in the transaction.
    | SlotAdd Slot
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ChainEvent where
    pretty = \case
        TxnValidate i _ _             -> "TxnValidate" <+> pretty i
        TxnValidationFail p i _ e _ _ -> "TxnValidationFail" <+> pretty p <+> pretty i <> colon <+> pretty e
        SlotAdd sl                    -> "SlotAdd" <+> pretty sl

-- | A pool of transactions which have yet to be validated.
type TxPool = [CardanoTx]

data ChainState = ChainState {
    _chainNewestFirst :: Blockchain, -- ^ The current chain, with the newest transactions first in the list.
    _txPool           :: TxPool, -- ^ The pool of pending transactions.
    _index            :: Index.UtxoIndex, -- ^ The UTxO index, used for validation.
    _currentSlot      :: Slot -- ^ The current slot number
} deriving (Show, Generic)

emptyChainState :: ChainState
emptyChainState = ChainState [] [] mempty 0

makeLenses ''ChainState

data ChainControlEffect r where
    ProcessBlock :: ChainControlEffect Block
    ModifySlot :: (Slot -> Slot) -> ChainControlEffect Slot

data ChainEffect r where
    QueueTx :: CardanoTx -> ChainEffect ()
    GetCurrentSlot :: ChainEffect Slot
    GetParams :: ChainEffect Params

type ChainEffs = '[State ChainState, LogMsg ChainEvent]

makePrisms ''ChainEvent
