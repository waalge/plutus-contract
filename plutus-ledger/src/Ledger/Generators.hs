{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Generators for constructing blockchains and transactions for use in property-based testing.
module Ledger.Generators(
    -- * Mockchain
    Mockchain(..),
    genMockchain,
    genMockchain',
    emptyChain,
    GeneratorModel(..),
    TxInputWitnessed(..),
    generatorModel,
    -- * Transactions
    genValidTransaction,
    genValidTransaction',
    genValidTransactionSpending,
    genValidTransactionSpending',
    genInitialTransaction,
    genValidatorContext,
    genMintingPolicyContext,
    -- * Assertions
    assertValid,
    -- * Time
    genInterval,
    genSlotRange,
    genTimeRange,
    genSlot,
    genPOSIXTime,
    genSlotConfig,
    -- * Etc.
    genSomeCardanoApiTx,
    genAda,
    genValue,
    genValueNonNegative,
    genSizedByteString,
    genSizedByteStringExact,
    genTokenName,
    genSeed,
    genPassphrase,
    splitVal,
    validateMockchain,
    signAll,
    knownPaymentPublicKeys,
    knownPaymentPrivateKeys,
    knownXPrvs,
    someTokenValue,
    genTxInfo
    ) where

import Cardano.Api qualified as C
import Cardano.Crypto.Wallet qualified as Crypto
import Control.Applicative ((<|>))
import Control.Lens ((&))
import Control.Monad (replicateM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Writer (runWriter)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Foldable (fold, foldl')
import Data.Functor.Identity (Identity)
import Data.List (sort)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (Ada, CardanoTx (EmulatorTx), CurrencySymbol, Datum, Interval, Language (PlutusV1), OnChainTx (Valid),
               POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange, Passphrase (Passphrase),
               PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey (PaymentPubKey), ScriptContext (ScriptContext),
               Slot (Slot), SlotRange, SomeCardanoApiTx (CardanoApiEmulatorEraTx, SomeTx), TokenName,
               Tx (txFee, txInputs, txMint, txOutputs, txValidRange), TxInInfo (txInInfoOutRef),
               TxInType (ConsumePublicKeyAddress), TxInfo (TxInfo), TxInput (TxInput),
               TxInputType (TxConsumePublicKeyAddress), TxOut, TxOutRef (TxOutRef), UtxoIndex (UtxoIndex),
               ValidationCtx (ValidationCtx), Validator, Value, Versioned, _runValidation, addCardanoTxSignature,
               addMintingPolicy, getValidator, pubKeyTxOut, scriptHash, toPublicKey, txData, txOutValue, txScripts,
               validatorHash)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CW
import Ledger.Index qualified as Index
import Ledger.Params (Params (pSlotConfig))
import Ledger.TimeSlot (SlotConfig)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Validation qualified as Validation
import Ledger.Value qualified as Value
import Plutus.Script.Utils.Scripts (Versioned (Versioned), datumHash)
import Plutus.Script.Utils.V1.Generators as ScriptGen
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts qualified as Script

-- | Attach signatures of all known private keys to a transaction.
signAll :: CardanoTx -> CardanoTx
signAll tx = foldl' (flip addCardanoTxSignature) tx
           $ fmap unPaymentPrivateKey knownPaymentPrivateKeys

-- | The parameters for the generators in this module.
data GeneratorModel = GeneratorModel {
    gmInitialBalance :: Map PaymentPubKey Value,
    -- ^ Value created at the beginning of the blockchain.
    gmPubKeys        :: Set PaymentPubKey
    -- ^ Public keys that are to be used for generating transactions.
    } deriving Show

-- | A generator model with some sensible defaults.
generatorModel :: GeneratorModel
generatorModel =
    let vl = Ada.lovelaceValueOf 100_000_000
        pubKeys = knownPaymentPublicKeys

    in
    GeneratorModel
    { gmInitialBalance = Map.fromList $ zip pubKeys (repeat vl)
    , gmPubKeys        = Set.fromList pubKeys
    }

-- | Blockchain for testing the emulator implementation and traces.
--
--   To avoid having to rely on functions from the implementation of
--   plutus-ledger (in particular, 'Ledger.Tx.unspentOutputs') we note the
--   unspent outputs of the chain when it is first created.
data Mockchain = Mockchain {
    mockchainInitialTxPool :: [CardanoTx],
    mockchainUtxo          :: Map TxOutRef TxOut,
    mockchainParams        :: Params
    } deriving Show

-- | The empty mockchain.
emptyChain :: Mockchain
emptyChain = Mockchain [] Map.empty def

-- | Generate a mockchain.
--
--   TODO: Generate more than 1 txn
genMockchain' :: MonadGen m
    => GeneratorModel
    -> m Mockchain
genMockchain' gm = do
    let (txn, ot) = genInitialTransaction gm
        tid = Ledger.getCardanoTxId txn
    slotCfg <- genSlotConfig
    pure Mockchain {
        mockchainInitialTxPool = [txn],
        mockchainUtxo = Map.fromList $ first (TxOutRef tid) <$> zip [0..] ot,
        mockchainParams = def { pSlotConfig = slotCfg }
        }

-- | Generate a mockchain using the default 'GeneratorModel'.
--
genMockchain :: MonadGen m => m Mockchain
genMockchain = genMockchain' generatorModel

-- | A transaction with no inputs that mints some value (to be used at the
--   beginning of a blockchain).
genInitialTransaction ::
       GeneratorModel
    -> (CardanoTx, [TxOut])
genInitialTransaction GeneratorModel{..} =
    let o = either (error . ("Cannot create outputs: " <>) . show) id
          $ traverse (\(ppk, v) -> pubKeyTxOut v ppk Nothing) $ Map.toList gmInitialBalance
        t = fold gmInitialBalance
    in (EmulatorTx $ mempty {
        txOutputs = o,
        txMint = t,
        txValidRange = Interval.from 0
        }, o)

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the minimum fee.
genValidTransaction :: MonadGen m
    => Mockchain
    -> m CardanoTx
genValidTransaction = genValidTransaction' generatorModel

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the estimated fee.
genValidTransaction' :: MonadGen m
    => GeneratorModel
    -> Mockchain
    -> m CardanoTx
genValidTransaction' g (Mockchain _ ops _) = do
    -- Take a random number of UTXO from the input
    nUtxo <- if Map.null ops
                then Gen.discard
                else Gen.int (Range.linear 1 (Map.size ops))
    let ins = (`TxInputWitnessed` ConsumePublicKeyAddress) . fst <$> inUTXO
        inUTXO = take nUtxo $ Map.toList ops
        totalVal = foldl' (<>) mempty $ map (txOutValue . snd) inUTXO
    genValidTransactionSpending' g ins totalVal

genValidTransactionSpending :: MonadGen m
    => [TxInputWitnessed]
    -> Value
    -> m CardanoTx
genValidTransactionSpending = genValidTransactionSpending' generatorModel

-- | A transaction input, consisting of a transaction output reference and an input type with data witnesses.
data TxInputWitnessed = TxInputWitnessed !TxOutRef !Ledger.TxInType

genValidTransactionSpending' :: MonadGen m
    => GeneratorModel
    -> [TxInputWitnessed]
    -> Value
    -> m CardanoTx
genValidTransactionSpending' g ins totalVal = do
    mintAmount <- toInteger <$> Gen.int (Range.linear 0 maxBound)
    mintTokenName <- genTokenName
    let mintValue = if mintAmount == 0
                       then Nothing
                       else Just $ ScriptGen.someTokenValue mintTokenName mintAmount
        fee' = Ada.lovelaceOf 10
        numOut = Set.size (gmPubKeys g) - 1
        totalValAda = Ada.fromValue totalVal
        totalValTokens = if Value.isZero (Value.noAdaValue totalVal) then Nothing else Just (Value.noAdaValue totalVal)
    if fee' < totalValAda
        then do
            -- We only split the Ada part of the input value
            splitOutVals <- splitVal numOut (totalValAda - fee')
            let outVals = case totalValTokens <> mintValue of
                  Nothing -> do
                    fmap Ada.toValue splitOutVals
                  Just mv -> do
                    -- If there is a minted value, we look for a value in the
                    -- splitted values which can be associated with it.
                    let outValForMint =
                          maybe mempty id $ List.find (\v -> v >= Ledger.minAdaTxOut)
                                          $ List.sort splitOutVals
                    Ada.toValue outValForMint <> mv : fmap Ada.toValue (List.delete outValForMint splitOutVals)
                txOutputs = either (error . ("Cannot create outputs: " <>) . show) id
                          $ traverse (\(v, ppk) -> pubKeyTxOut v ppk Nothing) $ zip outVals (Set.toList $ gmPubKeys g)
                (ins', witnesses) = unzip $ map txInToTxInput ins
                (scripts, datums) = unzip $ catMaybes witnesses
                tx = mempty
                        { txInputs = ins'
                        , txOutputs = txOutputs
                        , txMint = maybe mempty id mintValue
                        , txFee = Ada.toValue fee'
                        , txData = Map.fromList (map (\d -> (datumHash d, d)) datums)
                        , txScripts = Map.fromList (map ((\s -> (scriptHash s, s)) . fmap getValidator) scripts)
                        }
                    & addMintingPolicy (Versioned ScriptGen.alwaysSucceedPolicy PlutusV1) Script.unitRedeemer
                    & EmulatorTx

                -- sign the transaction with all known wallets
                -- this is somewhat crude (but technically valid)
            pure (signAll tx)
        else Gen.discard

    where
        -- | Translate TxIn to TxInput taking out data witnesses if present.
        txInToTxInput :: TxInputWitnessed -> (TxInput, Maybe (Versioned Validator, Datum))
        txInToTxInput (TxInputWitnessed outref txInType) = case txInType of
            Ledger.ConsumePublicKeyAddress -> (TxInput outref TxConsumePublicKeyAddress, Nothing)
            Ledger.ConsumeSimpleScriptAddress -> (TxInput outref Ledger.TxConsumeSimpleScriptAddress, Nothing)
            Ledger.ConsumeScriptAddress vl rd dt -> (TxInput outref (Ledger.TxConsumeScriptAddress rd (validatorHash  vl) (datumHash dt)), Just (vl, dt))

-- | Generate an 'Interval where the lower bound if less or equal than the
-- upper bound.
genInterval :: (MonadFail m, Ord a)
            => m a
            -> m (Interval a)
genInterval gen = do
    [b, e] <- sort <$> replicateM 2 gen
    return $ Interval.interval b e

-- | Generate a 'SlotRange' where the lower bound if less or equal than the
-- upper bound.
genSlotRange :: (MonadFail m, Hedgehog.MonadGen m) => m SlotRange
genSlotRange = genInterval genSlot

-- | Generate a 'POSIXTimeRange' where the lower bound if less or equal than the
-- upper bound.
genTimeRange :: (MonadFail m, Hedgehog.MonadGen m) => SlotConfig -> m POSIXTimeRange
genTimeRange sc = genInterval $ genPOSIXTime sc

-- | Generate a 'Slot' where the lowest slot number is 0.
genSlot :: (Hedgehog.MonadGen m) => m Slot
genSlot = Slot <$> Gen.integral (Range.linear 0 10000)

-- | Generate a 'POSIXTime' where the lowest value is 'scSlotZeroTime' given a
-- 'SlotConfig'.
genPOSIXTime :: (Hedgehog.MonadGen m) => SlotConfig -> m POSIXTime
genPOSIXTime sc = do
    let beginTime = getPOSIXTime $ TimeSlot.scSlotZeroTime sc
    POSIXTime <$> Gen.integral (Range.linear beginTime (beginTime + 10000000))

-- | Generate a 'SlotConfig' where the slot length goes from 1 to 100000
-- ms and the time of Slot 0 is the default 'scSlotZeroTime'.
genSlotConfig :: Hedgehog.MonadGen m => m SlotConfig
genSlotConfig = do
    sl <- Gen.integral (Range.linear 1 1000000)
    return $ def { TimeSlot.scSlotLength = sl }

-- TODO Unfortunately, there's no way to get a warning if another era has been
-- added to EraInMode. Alternative way?
genSomeCardanoApiTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genSomeCardanoApiTx = Gen.choice [ genByronEraInCardanoModeTx
                                 , genShelleyEraInCardanoModeTx
                                 , genAllegraEraInCardanoModeTx
                                 , genMaryEraInCardanoModeTx
                                 , genBabbageEraInCardanoModeTx
                                 ]

genByronEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genByronEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ByronEra
  pure $ SomeTx tx C.ByronEraInCardanoMode

genShelleyEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genShelleyEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyEra
  pure $ SomeTx tx C.ShelleyEraInCardanoMode

genAllegraEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genAllegraEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.AllegraEra
  pure $ SomeTx tx C.AllegraEraInCardanoMode

genMaryEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genMaryEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.MaryEra
  pure $ SomeTx tx C.MaryEraInCardanoMode

genBabbageEraInCardanoModeTx :: (GenBase m ~ Identity, MonadGen m) => m SomeCardanoApiTx
genBabbageEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.BabbageEra
  pure $ SomeTx tx C.BabbageEraInCardanoMode

genAda :: MonadGen m => m Ada
genAda = Ada.lovelaceOf <$> Gen.integral (Range.linear 0 (100000 :: Integer))

-- | Generate a 'ByteString s' of up to @s@ bytes.
genSizedByteString :: forall m. MonadGen m => Int -> m BS.ByteString
genSizedByteString s =
    let range = Range.linear 0 s
    in Gen.bytes range

-- | Generate a 'ByteString s' of exactly @s@ bytes.
genSizedByteStringExact :: forall m. MonadGen m => Int -> m BS.ByteString
genSizedByteStringExact s =
    let range = Range.singleton s
    in Gen.bytes range

-- | A TokenName is either an arbitrary bytestring or the ada token name
genTokenName :: MonadGen m => m TokenName
genTokenName = Gen.choice
    [ Value.tokenName <$> genSizedByteString 32
    , pure Ada.adaToken
    ]

-- | A currency symbol is either a validator hash (bytestring of length 32)
-- or the ada symbol (empty bytestring).
genCurrencySymbol :: MonadGen m => m CurrencySymbol
genCurrencySymbol = Gen.choice
    [ Value.currencySymbol <$> genSizedByteStringExact 32
    , pure Ada.adaSymbol
    ]

genValue' :: MonadGen m => Range Integer -> m Value
genValue' valueRange = do
    let
        sngl = Value.singleton <$> genCurrencySymbol <*> genTokenName <*> Gen.integral valueRange

        -- generate values with no more than 5 elements to avoid the tests
        -- taking too long (due to the map-as-list-of-kv-pairs implementation)
        maxCurrencies = 5

    numValues <- Gen.int (Range.linear 0 maxCurrencies)
    fold <$> traverse (const sngl) [0 .. numValues]

-- | Generate a 'Value' with a value range of @minBound .. maxBound@.
genValue :: MonadGen m => m Value
genValue = genValue' $ fromIntegral <$> Range.linearBounded @Int

-- | Generate a 'Value' with a value range of @0 .. maxBound@.
genValueNonNegative :: MonadGen m => m Value
genValueNonNegative = genValue' $ fromIntegral <$> Range.linear @Int 0 maxBound

-- | Assert that a transaction is valid in a chain.
assertValid :: (MonadTest m, HasCallStack)
    => CardanoTx
    -> Mockchain
    -> m ()
assertValid tx mc = Hedgehog.assert $ isNothing $ validateMockchain mc tx

-- | Validate a transaction in a mockchain.
validateMockchain :: Mockchain -> CardanoTx -> Maybe Index.ValidationError
validateMockchain (Mockchain txPool _ params) cardanoTx = result where
    h      = 1
    idx    = Index.initialise [map Valid txPool]
    cUtxoIndex = either (error . show) id $ Validation.fromPlutusIndex idx
    ctx = ValidationCtx idx params
    (err, _) = cardanoTx & Ledger.mergeCardanoTxWith
            (\tx -> Index.runValidation (Index.validateTransaction h tx) ctx)
            (\(CardanoApiEmulatorEraTx tx) -> (Validation.hasValidationErrors params (fromIntegral h) cUtxoIndex tx, []))
            (\(e1, sve1) (e2, sve2) -> (e1 <|> e2, sve1 ++ sve2))
    result = fmap snd err

{- | Split a value into max. n positive-valued parts such that the sum of the
     parts equals the original value. Each part should contain the required
     minimum amount of Ada.

     I noticed how for values of `mx` > 1000 the resulting lists are much smaller than
     one would expect. I think this may be caused by the way we select the next value
     for the split. It looks like the available funds get exhausted quite fast, which
     makes the function return before generating anything close to `mx` values.
-}
splitVal :: (MonadGen m, Integral n) => Int -> n -> m [n]
splitVal _  0     = pure []
splitVal mx init' = go 0 0 [] where
    go i c l =
        if i >= pred mx || init' - c < 2 * minAda
        then pure $ (init' - c) : l
        else do
            v <- Gen.integral (Range.linear minAda $ init' - c - minAda)
            if v + c == init'
            then pure $ v : l
            else go (succ i) (v + c) (v : l)
    minAda = fromIntegral $ Ada.getLovelace $ Ledger.minAdaTxOut + Ledger.maxFee

genTxInfo :: MonadGen m => Mockchain -> m TxInfo
genTxInfo chain = do
    cardanoTx <- genValidTransaction chain
    let tx = Ledger.onCardanoTx id (\_ -> error "Unexpected SomeCardanoApiTx") cardanoTx
        idx = UtxoIndex $ mockchainUtxo chain
        params = mockchainParams chain
        (res, _) = runWriter $ runExceptT $ runReaderT (_runValidation (Index.mkPV1TxInfo tx)) (ValidationCtx idx params)
    either (const Gen.discard) pure res

genScriptPurposeSpending :: MonadGen m => TxInfo -> m Contexts.ScriptPurpose
genScriptPurposeSpending TxInfo{txInfoInputs} = Gen.element $ Contexts.Spending . txInInfoOutRef <$> txInfoInputs

genScriptPurposeMinting :: MonadGen m => TxInfo -> m Contexts.ScriptPurpose
genScriptPurposeMinting TxInfo{txInfoMint} = Gen.element $ Contexts.Minting <$> Value.symbols txInfoMint

-- TODO: add Rewarding and Certifying purposes

genValidatorContext :: MonadGen m => Mockchain -> m ScriptContext
genValidatorContext chain = do
    txInfo <- genTxInfo chain
    purpose <- genScriptPurposeSpending txInfo
    pure $ ScriptContext txInfo purpose

genMintingPolicyContext :: MonadGen m => Mockchain -> m ScriptContext
genMintingPolicyContext chain = do
    txInfo <- genTxInfo chain
    purpose <- genScriptPurposeMinting txInfo
    pure $ ScriptContext txInfo purpose

knownPaymentPublicKeys :: [PaymentPubKey]
knownPaymentPublicKeys =
    PaymentPubKey . toPublicKey . unPaymentPrivateKey <$> knownPaymentPrivateKeys

knownPaymentPrivateKeys :: [PaymentPrivateKey]
knownPaymentPrivateKeys = CW.paymentPrivateKey <$> CW.knownMockWallets

knownXPrvs :: [Crypto.XPrv]
knownXPrvs = unPaymentPrivateKey <$> knownPaymentPrivateKeys

-- | Seed suitable for testing a seed but not for actual wallets as ScrubbedBytes isn't used to ensure
--  memory isn't inspectable
genSeed :: MonadGen m => m BS.ByteString
genSeed =  Gen.bytes $ Range.singleton 32

genPassphrase :: MonadGen m => m Passphrase
genPassphrase =
  Passphrase <$> Gen.utf8 (Range.singleton 16) Gen.unicode
