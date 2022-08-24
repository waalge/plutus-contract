{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
-- | Turn 'UnbalancedTx' values into transactions using the
--   wallet API.
module Plutus.Contract.Wallet(
      balanceTx
    , handleTx
    , yieldUnbalancedTx
    , getUnspentOutput
    , WAPI.signTxAndSubmit
    -- * Exporting transactions
    , ExportTx(..)
    , ExportTxInput(..)
    , ExportTxRedeemer(..)
    , export
    , finalize
    ) where

import Cardano.Api qualified as C
import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (^.))
import Control.Monad (join, (>=>))
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (String), object, withObject, (.:), (.=))
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Types (Parser, parseFail)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Semigroup qualified as Semigroup
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger qualified as P
import Ledger.Ada qualified as Ada
import Ledger.Constraints (mustPayToPubKey)
import Ledger.Constraints.OffChain (UnbalancedTx (UnbalancedTx, unBalancedTxRequiredSignatories, unBalancedTxTx, unBalancedTxUtxoIndex),
                                    mkTx)
import Ledger.Constraints.OffChain qualified as U
import Ledger.TimeSlot (SlotConfig, posixTimeRangeToContainedSlotRange)
import Ledger.Tx (CardanoTx, TxId (TxId), TxIn (..), TxInType (..), TxOutRef, getCardanoTxInputs, txInRef)
import Ledger.Validation (CardanoLedgerError, fromPlutusIndex, makeTransactionBody)
import Plutus.Contract.CardanoAPI qualified as CardanoAPI
import Plutus.Contract.Error (AsContractError (_ConstraintResolutionContractError, _OtherContractError))
import Plutus.Contract.Request qualified as Contract
import Plutus.Contract.Types (Contract)
import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import Plutus.V1.Ledger.Tx qualified as PV1
import PlutusTx qualified
import Wallet.API qualified as WAPI
import Wallet.Effects (WalletEffect, balanceTx, yieldUnbalancedTx)
import Wallet.Emulator.Error (WalletAPIError)
