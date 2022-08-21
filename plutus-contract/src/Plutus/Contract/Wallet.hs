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

import qualified Cardano.Api                 as C
import           Control.Applicative         ((<|>))
import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad               (join, (>=>))
import           Control.Monad.Error.Lens    (throwing)
import           Control.Monad.Freer         (Eff, Member)
import           Control.Monad.Freer.Error   (Error, throwError)
import           Data.Aeson                  (FromJSON (parseJSON), Object,
                                              ToJSON (toJSON), Value (String),
                                              object, withObject, (.:), (.=))
import qualified Data.Aeson.Extras           as JSON
import           Data.Aeson.Types            (Parser, parseFail)
import           Data.Bifunctor              (first)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, mapMaybe)
import qualified Data.OpenApi                as OpenApi
import qualified Data.Semigroup              as Semigroup
import qualified Data.Set                    as Set
import           Data.Typeable               (Typeable)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import qualified Ledger                      as P
import qualified Ledger.Ada                  as Ada
import           Ledger.Constraints          (mustPayToPubKey)
import           Ledger.Constraints.OffChain (UnbalancedTx (UnbalancedTx, unBalancedTxRequiredSignatories, unBalancedTxTx, unBalancedTxUtxoIndex),
                                              mkTx)
import qualified Ledger.Constraints.OffChain as U
import           Ledger.TimeSlot             (SlotConfig,
                                              posixTimeRangeToContainedSlotRange)
import           Ledger.Tx                   (CardanoTx, TxId (TxId), TxIn (..),
                                              TxInType (..), TxOutRef,
                                              getCardanoTxInputs, txInRef)
import           Ledger.Validation           (CardanoLedgerError,
                                              fromPlutusIndex,
                                              makeTransactionBody)
import qualified Plutus.Contract.CardanoAPI  as CardanoAPI
import           Plutus.Contract.Error       (AsContractError (_ConstraintResolutionContractError, _OtherContractError))
import qualified Plutus.Contract.Request     as Contract
import           Plutus.Contract.Types       (Contract)
import qualified Plutus.V1.Ledger.Api        as Plutus
import           Plutus.V1.Ledger.Scripts    (MintingPolicyHash)
import qualified Plutus.V1.Ledger.Tx         as PV1
import qualified PlutusTx
import qualified Wallet.API                  as WAPI
import           Wallet.Effects              (WalletEffect, balanceTx,
                                              yieldUnbalancedTx)
import           Wallet.Emulator.Error       (WalletAPIError)
