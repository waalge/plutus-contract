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
    -- * Dealing with time
    , Request.awaitSlot
    , Request.isSlot
    , Request.currentSlot
    , Request.currentPABSlot
    , Request.currentChainIndexSlot
    , Request.waitNSlots
    , Request.awaitTime
    , Request.isTime
    , Request.currentTime
    , Request.waitNMilliSeconds
    -- * Endpoints
    , Request.HasEndpoint
    , Request.EndpointDescription(..)
    , Request.Endpoint
    , Request.endpoint
    , Request.handleEndpoint
    , Request.endpointWithMeta
    , Schema.EmptySchema
    -- * Blockchain events
    , Request.watchAddressUntilSlot
    , Request.watchAddressUntilTime
    , Request.fundsAtAddressGt
    , Request.fundsAtAddressGeq
    , Request.awaitUtxoSpent
    , Request.utxoIsSpent
    , Request.awaitUtxoProduced
    , Request.utxoIsProduced
    -- * Chain index requests
    , Request.datumFromHash
    , Request.validatorFromHash
    , Request.mintingPolicyFromHash
    , Request.stakeValidatorFromHash
    , Request.txOutFromRef
    , Request.unspentTxOutFromRef
    , Request.txFromTxId
    , Request.utxoRefMembership
    , Request.utxoRefsAt
    , Request.utxoRefsWithCurrency
    , Request.utxosAt
    , Request.utxosTxOutTxFromTx
    , Request.getTip
    -- * Wallet's information
    , Request.ownPaymentPubKeyHash
    , Request.ownPaymentPubKeyHashes
    , Request.ownFirstPaymentPubKeyHash
    , Request.ownAddresses
    , Request.ownUtxos
    -- * Contract instance Id
    , Wallet.Types.ContractInstanceId
    , Request.ownInstanceId
    -- * Notifications
    , tell
    -- * Transactions
    , WalletAPIError
    , Request.adjustUnbalancedTx
    , Request.submitTx
    , Request.submitTxConfirmed
    , Request.submitTxConstraints
    , Request.submitTxConstraintsSpending
    , Request.submitTxConstraintsWith
    , Request.submitUnbalancedTx
    , Request.submitBalancedTx
    , Request.balanceTx
    , Request.mkTxConstraints
    , Request.yieldUnbalancedTx
    -- ** Tx confirmation
    , Request.awaitTxConfirmed
    , Request.awaitTxStatusChange
    , Request.isTxConfirmed
    -- ** Tx output confirmation
    , Request.awaitTxOutStatusChange
    -- * Checkpoints
    , Plutus.Contract.Types.checkpoint
    , Plutus.Contract.Types.checkpointLoop
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

import           Data.Row                   (Empty, HasType, type (.\/))

import           Plutus.Contract.Logging    as Logging
import           Plutus.Contract.Request    (ContractRow)
import qualified Plutus.Contract.Request    as Request
import qualified Plutus.Contract.Schema     as Schema
import           Plutus.Contract.Types      (Contract (Contract), Promise,
                                             select)
import qualified Plutus.Contract.Types

import qualified Control.Monad.Freer.Writer as W
import           Data.Functor.Apply         (liftF2)
import           Wallet.API                 (WalletAPIError)
import qualified Wallet.Types

-- | Execute both contracts in any order
both :: Promise w s e a -> Promise w s e b -> Promise w s e (a, b)
both a b = liftF2 (,) a b `select` liftF2 (flip (,)) b a

-- | Update the contract's accumulating state @w@
tell :: w -> Contract w s e ()
tell = Contract . W.tell
