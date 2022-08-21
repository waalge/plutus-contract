--NOTE: all the functions went bathing
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Plutus.Contract.Request(
    -- * PAB requests
    -- ** Waiting
    -- ** Tx and tx output confirmation
      RollbackState(..)
    , TxStatus
    , TxOutStatus
    -- ** Exposing endpoints
    , HasEndpoint
    , EndpointDescription(..)
    , Endpoint
    -- * Etc.
    , ContractRow
    , MkTxLog(..)
    ) where

import           Control.Lens                     (Prism', preview, review,
                                                   view)
import qualified Control.Monad.Freer.Error        as E
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as JSON
import qualified Data.Aeson.Types                 as JSON
import           Data.Bifunctor                   (Bifunctor (..))
import           Data.Default                     (Default (def))
import           Data.List.NonEmpty               (NonEmpty)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, mapMaybe)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Row                         (AllUniqueLabels, HasType,
                                                   KnownSymbol, type (.==))
import qualified Data.Text                        as Text
import           Data.Text.Extras                 (tshow)
import           Data.Void                        (Void)
import           GHC.Generics                     (Generic)
import           GHC.Natural                      (Natural)
import           GHC.TypeLits                     (Symbol, symbolVal)
import           Ledger                           (AssetClass, DiffMilliSeconds,
                                                   POSIXTime,
                                                   PaymentPubKeyHash (PaymentPubKeyHash),
                                                   Slot, TxId, TxOutRef, Value,
                                                   addressCredential,
                                                   fromMilliSeconds, txOutRefId)
import           Ledger.Constraints               (TxConstraints)
import           Ledger.Constraints.OffChain      (ScriptLookups, UnbalancedTx)
import qualified Ledger.Constraints.OffChain      as Constraints
import           Ledger.Tx                        (CardanoTx, ChainIndexTxOut,
                                                   ciTxOutValue, getCardanoTxId)
import           Ledger.Typed.Scripts             (Any, TypedValidator,
                                                   ValidatorTypes (DatumType, RedeemerType))
import qualified Ledger.Value                     as V
import           Plutus.Contract.Util             (loopM)
import           Plutus.V1.Ledger.Api             (Address, Datum, DatumHash,
                                                   MintingPolicy,
                                                   MintingPolicyHash, Redeemer,
                                                   RedeemerHash, StakeValidator,
                                                   StakeValidatorHash,
                                                   Validator, ValidatorHash)
import qualified PlutusTx

import           Plutus.Contract.Effects          (ActiveEndpoint (ActiveEndpoint, aeDescription, aeMetadata),
                                                   PABReq (AdjustUnbalancedTxReq, AwaitSlotReq, AwaitTimeReq, AwaitTxOutStatusChangeReq, AwaitTxStatusChangeReq, AwaitUtxoProducedReq, AwaitUtxoSpentReq, BalanceTxReq, ChainIndexQueryReq, CurrentChainIndexSlotReq, CurrentPABSlotReq, CurrentTimeReq, ExposeEndpointReq, OwnAddressesReq, OwnContractInstanceIdReq, WriteBalancedTxReq, YieldUnbalancedTxReq),
                                                   PABResp (ExposeEndpointResp))
import qualified Plutus.Contract.Effects          as E
import           Plutus.Contract.Logging          (logDebug)
import           Plutus.Contract.Schema           (Input, Output)
import           Wallet.Types                     (ContractInstanceId,
                                                   EndpointDescription (EndpointDescription),
                                                   EndpointValue (EndpointValue, unEndpointValue))

import           Data.Foldable                    (fold)
import qualified Data.List.NonEmpty               as NonEmpty
import           Plutus.ChainIndex                (ChainIndexTx,
                                                   Page (nextPageQuery, pageItems),
                                                   PageQuery, txOutRefs)
import           Plutus.ChainIndex.Api            (IsUtxoResponse,
                                                   QueryResponse, TxosResponse,
                                                   UtxosResponse,
                                                   collectQueryResponse, paget)
import           Plutus.ChainIndex.Types          (RollbackState (Unknown), Tip,
                                                   TxOutStatus, TxStatus)
import           Plutus.Contract.Error            (AsContractError (_ChainIndexContractError, _ConstraintResolutionContractError, _EndpointDecodeContractError, _ResumableContractError, _TxToCardanoConvertContractError, _WalletContractError))
import           Plutus.Contract.Resumable        (prompt)
import           Plutus.Contract.Types            (Contract (Contract),
                                                   MatchingError (WrongVariantError),
                                                   Promise (Promise), mapError,
                                                   runError, throwError)
import           Plutus.V1.Ledger.Address         (toPubKeyHash)
import           Wallet.Emulator.Error            (WalletAPIError (NoPaymentPubKeyHashError))

-- | Constraints on the contract schema, ensuring that the labels of the schema
--   are unique.
type ContractRow s =
  ( AllUniqueLabels (Input s)
  , AllUniqueLabels (Output s)
  )

type HasEndpoint l a s =
  ( HasType l (EndpointValue a) (Input s)
  , HasType l ActiveEndpoint (Output s)
  , KnownSymbol l
  , ContractRow s
  )

type Endpoint l a = l .== (EndpointValue a, ActiveEndpoint)

{-| Arguments and result of a call to 'mkTx'
-}
data MkTxLog =
    MkTxLog
        { mkTxLogLookups       :: ScriptLookups Any
        , mkTxLogTxConstraints :: TxConstraints PlutusTx.BuiltinData PlutusTx.BuiltinData
        , mkTxLogResult        :: Either Constraints.MkTxError UnbalancedTx
        }
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON)
