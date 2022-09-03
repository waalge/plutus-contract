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
    -- ** Wallet information
    , ownAddresses
    , ownPaymentPubKeyHashes
    , ownFirstPaymentPubKeyHash
    -- ** Submitting transactions
    , adjustUnbalancedTx
    , balanceTx
    -- * Etc.
    , ContractRow
    , MkTxLog(..)
    , pabReq
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
import Plutus.V1.Ledger.Api (Address, Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash,
                             StakeValidator, StakeValidatorHash, Validator, ValidatorHash)
import PlutusTx qualified

import Plutus.Contract.Effects (ActiveEndpoint (ActiveEndpoint, aeDescription, aeMetadata),
                                PABReq (AdjustUnbalancedTxReq, AwaitSlotReq, AwaitTimeReq, AwaitTxOutStatusChangeReq, AwaitTxStatusChangeReq, AwaitUtxoProducedReq, AwaitUtxoSpentReq, BalanceTxReq, ChainIndexQueryReq, CurrentChainIndexSlotReq, CurrentPABSlotReq, CurrentTimeReq, ExposeEndpointReq, OwnAddressesReq, OwnContractInstanceIdReq, WriteBalancedTxReq, YieldUnbalancedTxReq),
                                PABResp (ExposeEndpointResp))
import Plutus.Contract.Effects qualified as E
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
                              throwError)
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


{- Send a 'PABReq' and return the appropriate 'PABResp'
-}
pabReq ::
  forall w s e a.
  ( AsContractError e
  )
  => PABReq -- ^ The request to send
  -> Prism' PABResp a -- ^ Prism for the response
  -> Contract w s e a
pabReq req prism = Contract $ do
  x <- prompt @PABResp @PABReq req
  case preview prism x of
    Just r -> pure r
    _      ->
        E.throwError @e
            $ review _ResumableContractError
            $ WrongVariantError
            $ "unexpected answer: " <> tshow x

-- | Adjust the unbalanced tx
adjustUnbalancedTx ::
    forall w s e.
    ( AsContractError e
    )
    => UnbalancedTx
    -> Contract w s e UnbalancedTx
adjustUnbalancedTx utx =
  let req = pabReq (AdjustUnbalancedTxReq utx) E._AdjustUnbalancedTxResp in
  req >>= either (throwError . review _TxToCardanoConvertContractError) pure

-- | Get the addresses belonging to the wallet that runs this contract.
--   * Any funds paid to one of these addresses will be treated as the wallet's own
--     funds
--   * The wallet is able to sign transactions with the private key of one of its
--     public key, for example, if the public key is added to the
--     'requiredSignatures' field of 'Tx'.
--   * There is a 1-n relationship between wallets and addresses (although in
--     the mockchain n=1)
ownAddresses :: forall w s e. (AsContractError e) => Contract w s e (NonEmpty Address)
ownAddresses = pabReq OwnAddressesReq E._OwnAddressesResp

ownPaymentPubKeyHashes :: forall w s e. (AsContractError e) => Contract w s e [PaymentPubKeyHash]
ownPaymentPubKeyHashes = do
    addrs <- ownAddresses
    pure $ fmap PaymentPubKeyHash $ mapMaybe toPubKeyHash $ NonEmpty.toList addrs


ownFirstPaymentPubKeyHash :: forall w s e. (AsContractError e) => Contract w s e PaymentPubKeyHash
ownFirstPaymentPubKeyHash = do
    pkhs <- ownPaymentPubKeyHashes
    case pkhs of
      []      -> throwError $ review _WalletContractError NoPaymentPubKeyHashError
      (pkh:_) -> pure pkh

-- | Send an unbalanced transaction to be balanced. Returns the balanced transaction.
--    Throws an error if balancing failed.
balanceTx :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e CardanoTx
-- See Note [Injecting errors into the user's error type]
balanceTx t =
  let req = pabReq (BalanceTxReq t) E._BalanceTxResp in
  req >>= either (throwError . review _WalletContractError) pure . view E.balanceTxResponse


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
