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

import Control.Lens (Prism', preview, review, view)
import Control.Monad.Freer.Error qualified as E
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.Bifunctor (Bifunctor (..))
import Data.Default (Default (def))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Row (AllUniqueLabels, HasType, KnownSymbol, type (.==))
import Data.Text qualified as Text
import Data.Text.Extras (tshow)
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.TypeLits (Symbol, symbolVal)
import Ledger (AssetClass, DiffMilliSeconds, POSIXTime, PaymentPubKeyHash (PaymentPubKeyHash), Slot, TxId, TxOutRef,
               Value, addressCredential, fromMilliSeconds, txOutRefId)
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints.OffChain (ScriptLookups, UnbalancedTx)
import Ledger.Constraints.OffChain qualified as Constraints
import Ledger.Tx (CardanoTx, ChainIndexTxOut, ciTxOutValue, getCardanoTxId)
import Ledger.Typed.Scripts (Any, TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Value qualified as V
import Plutus.Contract.Util (loopM)
import Plutus.V1.Ledger.Api (Address, Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash,
                             StakeValidator, StakeValidatorHash, Validator, ValidatorHash)
import PlutusTx qualified

import Plutus.Contract.Effects (ActiveEndpoint (ActiveEndpoint, aeDescription, aeMetadata),
                                PABReq (AdjustUnbalancedTxReq, AwaitSlotReq, AwaitTimeReq, AwaitTxOutStatusChangeReq, AwaitTxStatusChangeReq, AwaitUtxoProducedReq, AwaitUtxoSpentReq, BalanceTxReq, ChainIndexQueryReq, CurrentChainIndexSlotReq, CurrentPABSlotReq, CurrentTimeReq, ExposeEndpointReq, OwnAddressesReq, OwnContractInstanceIdReq, WriteBalancedTxReq, YieldUnbalancedTxReq),
                                PABResp (ExposeEndpointResp))
import Plutus.Contract.Effects qualified as E
import Plutus.Contract.Logging (logDebug)
import Plutus.Contract.Schema (Input, Output)
import Wallet.Types (ContractInstanceId, EndpointDescription (EndpointDescription),
                     EndpointValue (EndpointValue, unEndpointValue))

import Data.Foldable (fold)
import Data.List.NonEmpty qualified as NonEmpty
import Plutus.ChainIndex (ChainIndexTx, Page (nextPageQuery, pageItems), PageQuery, txOutRefs)
import Plutus.ChainIndex.Api (IsUtxoResponse, QueryResponse, TxosResponse, UtxosResponse, collectQueryResponse, paget)
import Plutus.ChainIndex.Types (RollbackState (Unknown), Tip, TxOutStatus, TxStatus)
import Plutus.Contract.Error (AsContractError (_ChainIndexContractError, _ConstraintResolutionContractError, _EndpointDecodeContractError, _ResumableContractError, _TxToCardanoConvertContractError, _WalletContractError))
import Plutus.Contract.Resumable (prompt)
import Plutus.Contract.Types (Contract (Contract), MatchingError (WrongVariantError), Promise (Promise), mapError,
                              runError, throwError)
import Plutus.V1.Ledger.Address (toPubKeyHash)
import Wallet.Emulator.Error (WalletAPIError (NoPaymentPubKeyHashError))

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
