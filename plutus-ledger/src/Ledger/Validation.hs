{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-| Transaction validation using 'cardano-ledger-specs'
-}
module Ledger.Validation(
  EmulatorBlock,
  EmulatedLedgerState(..),
  Coin(..),
  SlotNo(..),
  EmulatorEra,
  CardanoLedgerError,
  initialState,
  evaluateMinLovelaceOutput,
  getRequiredSigners,
  addSignature,
  hasValidationErrors,
  makeTransactionBody,
  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  UTxO(..),
  setUtxo,
  -- * Conversion from Plutus types
  fromPlutusTx,
  fromPlutusIndex,
  fromPlutusTxOut,
  fromPlutusTxOutRef,
  -- * Lenses
  ledgerEnv,
  memPoolState,
  currentBlock,
  previousBlocks,
  -- * Etc.
  emulatorGlobals,
  ) where

import Cardano.Api.Shelley (ShelleyBasedEra (ShelleyBasedEraBabbage), makeSignedTransaction, toShelleyTxId)
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Ledger.Alonzo.PlutusScriptApi (collectTwoPhaseScriptInputs, evalScripts)
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (CollectErrors))
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits (ExUnits), Script, unCostModels)
import Cardano.Ledger.Alonzo.Tools qualified as C.Ledger
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), ScriptResult (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, txwitsVKey)
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Babbage (TxBody, TxOut)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Tx (IsValid (..), ValidatedTx (..))
import Cardano.Ledger.Babbage.TxBody (TxBody (TxBody, reqSignerHashes))
import Cardano.Ledger.BaseTypes (Globals (..), ProtVer, epochInfo, mkTxIxPartial)
import Cardano.Ledger.Core (Tx)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (Crypto, ValidateScript)
import Cardano.Ledger.Shelley.API (Coin (..), LedgerEnv (..), MempoolEnv, MempoolState, TxId, TxIn, UTxO (UTxO),
                                   Validated)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Lens (makeLenses, over, (&), (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Array (array)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Records (HasField (..))
import Ledger.Ada qualified as P
import Ledger.Address qualified as P
import Ledger.Index.Internal qualified as P
import Ledger.Params (EmulatorEra, emulatorGlobals, emulatorPParams)
import Ledger.Params qualified as P
import Ledger.Tx.CardanoAPI qualified as P
import Ledger.Tx.Internal qualified as P
import Plutus.V1.Ledger.Api qualified as V1 hiding (TxOut (..))
import Plutus.V1.Ledger.Scripts qualified as P
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.ErrorCodes (checkHasFailedError)

type CardanoLedgerError = Either P.ValidationErrorInPhase P.ToCardanoError

type EmulatorBlock = [Validated (Tx EmulatorEra)]

{- Note [Emulated ledger]

In the real cardano node, there two types of validation: Transaction validation
(performed when a transaction is first added to the mempool) and block
validation (performed when a block is created by the local node or received
from a peer).

Transaction validation runs the Plutus scripts, checks cryptographic
signatures, balances, existence of transaction inputs and so forth. This is
where the ledger state is updated. Block validation performs other checks
related to the consensus algorithm.

Networking and consensus issues are not part of the emulator's scope. We only
care about transaction validation here, so we don't have to worry about block
validation.

The decision to leave out block validation and consensus-related concerns has
the following implications:

1. We can represent blocks as simple lists-of-transactions
2. We can modify time (the slot number) and ledger parameters as we wish,
   without having to post transactions that modify them.

There are also some limitations of the emulator's functionality that could be
addressed by extending the emulator, without having to bring in the full block
validating machinery.

* We cannot represent different eras - everything is 'BabbageEra'.
* There is no handling of epoch boundaries, rewards, etc.
* The block size is unlimited - we simply take all transactions from the
  mempool when we make a block. There is however a limit on the size of
  individual transactions.
* We use the standard ledger cryptography everywhere ('StandardCrypto').
  This could be replaced by "NoCrypto" for faster validation.

-}

{-| State of the ledger with configuration, mempool, and the blockchain.
-}
data EmulatedLedgerState =
  EmulatedLedgerState
    { _ledgerEnv      :: MempoolEnv EmulatorEra
    , _memPoolState   :: MempoolState EmulatorEra
    , _currentBlock   :: EmulatorBlock
    , _previousBlocks :: [EmulatorBlock]
    }
    deriving Show

makeLenses ''EmulatedLedgerState

{-| Increase the slot number by one
-}
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over ledgerEnv f where
  f l@LedgerEnv{ledgerSlotNo=oldSlot} = l{ledgerSlotNo = succ oldSlot}

{-| Set the slot number
-}
setSlot :: SlotNo -> EmulatedLedgerState -> EmulatedLedgerState
setSlot sl = over ledgerEnv (\l -> l{ledgerSlotNo=sl})

{-| Set the utxo
-}
setUtxo :: UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
setUtxo utxo els@EmulatedLedgerState{_memPoolState} = els { _memPoolState = newPoolState }
  where
    newPoolState = _memPoolState { lsUTxOState = smartUTxOState utxo (Coin 0) (Coin 0) def }

{-| Make a block with all transactions that have been validated in the
current block, add the block to the blockchain, and empty the current block.
-}
makeBlock :: EmulatedLedgerState -> EmulatedLedgerState
makeBlock state =
  state
    & currentBlock .~ []
    & over previousBlocks ((:) (reverse $ state ^. currentBlock))

{-| Initial ledger state for a distribution
-}
initialState :: P.Params -> EmulatedLedgerState
initialState params = EmulatedLedgerState
  { _ledgerEnv = C.Ledger.LedgerEnv
      { C.Ledger.ledgerSlotNo = 0
      , C.Ledger.ledgerIx = minBound
      , C.Ledger.ledgerPp = C.Api.toLedgerPParams ShelleyBasedEraBabbage $ P.pProtocolParams params
      , C.Ledger.ledgerAccount = C.Ledger.AccountState (Coin 0) (Coin 0)
      }
  , _memPoolState = LedgerState
    { lsUTxOState = smartUTxOState (UTxO mempty) (Coin 0) (Coin 0) def
    , lsDPState = C.Ledger.DPState def def
    }
  , _currentBlock = []
  , _previousBlocks = []
  }


utxoEnv :: P.Params -> SlotNo -> C.Ledger.UtxoEnv EmulatorEra
utxoEnv params slotNo = C.Ledger.UtxoEnv slotNo (emulatorPParams params) mempty (C.Ledger.GenDelegs mempty)

applyTx ::
  P.Params ->
  EmulatedLedgerState ->
  Tx EmulatorEra ->
  Either P.ValidationError (EmulatedLedgerState, Validated (Tx EmulatorEra))
applyTx params oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- first (P.CardanoLedgerValidationError . show) (C.Ledger.applyTx (emulatorGlobals params) _ledgerEnv _memPoolState tx)
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)


hasValidationErrors :: P.Params -> SlotNo -> UTxO EmulatorEra -> C.Api.Tx C.Api.BabbageEra -> Maybe P.ValidationErrorInPhase
hasValidationErrors params slotNo utxo (C.Api.ShelleyTx _ tx) =
  case res of
    Left e  -> Just (P.Phase1, e)
    Right _ -> Nothing
  where
    state = setSlot slotNo $ setUtxo utxo $ initialState params
    res = do
      vtx <- first (P.CardanoLedgerValidationError . show) (constructValidated (emulatorGlobals params) (utxoEnv params slotNo) (lsUTxOState (_memPoolState state)) tx)
      applyTx params state vtx

-- | Construct a 'ValidatedTx' from a 'Core.Tx' by setting the `IsValid`
-- flag.
--
-- Note that this simply constructs the transaction; it does not validate
-- anything other than the scripts. Thus the resulting transaction may be
-- completely invalid.
--
-- Copied from cardano-ledger as it was removed there
-- in https://github.com/input-output-hk/cardano-ledger/commit/721adb55b39885847562437a6fe7e998f8e48c03
constructValidated ::
  forall era m.
  ( MonadError [UtxosPredicateFailure era] m,
    Core.Script era ~ Script era,
    Core.Witnesses era ~ Alonzo.TxWitness era,
    ValidateScript era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_costmdls" (Core.PParams era) CostModels,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    ExtendedUTxO era
  ) =>
  Globals ->
  UtxoEnv era ->
  UTxOState era ->
  Core.Tx era ->
  m (ValidatedTx era)
constructValidated globals (UtxoEnv _ pp _ _) st tx =
  case collectTwoPhaseScriptInputs ei sysS pp tx utxo of
    Left errs -> throwError [CollectErrors errs]
    Right sLst ->
      let scriptEvalResult = evalScripts @era (getField @"_protocolVersion" pp) tx sLst
          vTx =
            ValidatedTx
              (getField @"body" tx)
              (getField @"wits" tx)
              (IsValid (lift scriptEvalResult))
              (getField @"auxiliaryData" tx)
       in pure vTx
  where
    utxo = _utxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift (Passes _)  = True
    lift (Fails _ _) = False

getTxExUnits :: P.Params -> UTxO EmulatorEra -> C.Api.Tx C.Api.BabbageEra -> Either CardanoLedgerError (Map.Map RdmrPtr ExUnits)
getTxExUnits params utxo (C.Api.ShelleyTx _ tx) =
  case C.Ledger.evaluateTransactionExecutionUnits (emulatorPParams params) tx utxo ei ss costmdls of
    Left e      -> Left . Left . (P.Phase1,) . P.CardanoLedgerValidationError . show $ e
    Right rdmrs -> traverse (either toCardanoLedgerError Right) rdmrs
  where
    eg = emulatorGlobals params
    ss = systemStart eg
    ei = epochInfo eg
    costmdls = array (minBound, maxBound) . Map.toList $ unCostModels $ getField @"_costmdls" $ emulatorPParams params
    -- Failing transactions throw a checkHasFailedError error, but we don't want to deal with those yet.
    -- We might be able to do that in the future.
    -- But for now just return a zero execution cost so it will run later where we do handle failing transactions.
    toCardanoLedgerError (C.Ledger.ValidationFailedV1 (V1.CekError _) logs@(_:_)) | last logs == Builtins.fromBuiltin checkHasFailedError =
      Right $ ExUnits 0 0
    toCardanoLedgerError (C.Ledger.ValidationFailedV1 (V1.CekError ce) logs) =
      Left $ Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError (C.Ledger.ValidationFailedV2 (V1.CekError _) logs@(_:_)) | last logs == Builtins.fromBuiltin checkHasFailedError =
      Right $ ExUnits 0 0
    toCardanoLedgerError (C.Ledger.ValidationFailedV2 (V1.CekError ce) logs) =
      Left $ Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError e = Left $ Left (P.Phase2, P.CardanoLedgerValidationError (show e))

makeTransactionBody
  :: P.Params
  -> UTxO EmulatorEra
  -> P.CardanoBuildTx
  -> Either CardanoLedgerError (C.Api.TxBody C.Api.BabbageEra)
makeTransactionBody params utxo txBodyContent = do
  txTmp <- first Right $ makeSignedTransaction [] <$> P.makeTransactionBody mempty txBodyContent
  exUnits <- getTxExUnits params utxo txTmp
  first Right $ P.makeTransactionBody exUnits txBodyContent


evaluateMinLovelaceOutput :: P.Params -> TxOut EmulatorEra -> P.Ada
evaluateMinLovelaceOutput params = toPlutusValue . C.Ledger.evaluateMinLovelaceOutput (emulatorPParams params)
  where
    toPlutusValue :: Coin -> P.Ada
    toPlutusValue (Coin c) = P.lovelaceOf c

fromPlutusTx
  :: P.Params
  -> UTxO EmulatorEra
  -> [P.PaymentPubKeyHash]
  -> P.Tx
  -> Either CardanoLedgerError (C.Api.Tx C.Api.BabbageEra)
fromPlutusTx params utxo requiredSigners tx = do
  txBodyContent <- first Right $ P.toCardanoTxBodyContent params requiredSigners tx
  makeSignedTransaction [] <$> makeTransactionBody params utxo txBodyContent

getRequiredSigners :: C.Api.Tx C.Api.BabbageEra -> [P.PaymentPubKeyHash]
getRequiredSigners (C.Api.ShelleyTx _ (ValidatedTx TxBody { reqSignerHashes = rsq } _ _ _)) =
  foldMap (pure . P.PaymentPubKeyHash . P.fromCardanoPaymentKeyHash . C.Api.PaymentKeyHash . C.Ledger.coerceKeyRole) rsq

addSignature
  :: Crypto.XPrv
  -> C.Api.Tx C.Api.BabbageEra
  -> C.Api.Tx C.Api.BabbageEra
addSignature privKey (C.Api.ShelleyTx shelleyBasedEra (ValidatedTx body wits isValid aux))
    = C.Api.ShelleyTx shelleyBasedEra (ValidatedTx body wits' isValid aux)
  where
    wits' = wits <> mempty { txwitsVKey = newWits }
    newWits = case fromPaymentPrivateKey privKey body of
      C.Api.ShelleyKeyWitness _ wit -> Set.singleton wit
      _                             -> Set.empty

fromPlutusIndex :: P.UtxoIndex -> Either CardanoLedgerError (UTxO EmulatorEra)
fromPlutusIndex (P.UtxoIndex m) =
  first Right $ UTxO . Map.fromList <$> traverse (bitraverse fromPlutusTxOutRef (pure . fromPlutusTxOut)) (Map.toList m)

fromPlutusTxOutRef :: P.TxOutRef -> Either P.ToCardanoError (TxIn StandardCrypto)
fromPlutusTxOutRef (P.TxOutRef txId i) = TxIn <$> fromPlutusTxId txId <*> pure (mkTxIxPartial i)

fromPlutusTxId :: V1.TxId -> Either P.ToCardanoError (TxId StandardCrypto)
fromPlutusTxId = fmap toShelleyTxId . P.toCardanoTxId

fromPlutusTxOut :: P.TxOut -> TxOut EmulatorEra
fromPlutusTxOut = C.Api.toShelleyTxOut ShelleyBasedEraBabbage . C.Api.toCtxUTxOTxOut . P.getTxOut

fromPaymentPrivateKey :: Crypto.XPrv -> TxBody EmulatorEra -> C.Api.KeyWitness C.Api.BabbageEra
fromPaymentPrivateKey xprv txBody
  = C.Api.makeShelleyKeyWitness
      (C.Api.ShelleyTxBody C.Api.ShelleyBasedEraBabbage txBody notUsed notUsed notUsed notUsed)
      (C.Api.WitnessPaymentExtendedKey (C.Api.PaymentExtendedSigningKey xprv))
  where
    notUsed = undefined -- hack so we can reuse code from cardano-api
