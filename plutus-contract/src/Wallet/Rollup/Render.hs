{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Wallet.Rollup.Render(
    showBlockchain
    , showBlockchainFold
    ) where

import Codec.Serialise.Class (Serialise, decode, encode)
import Control.Lens.Combinators (itraverse)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BSS
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (Address, Blockchain, PaymentPubKey, PaymentPubKeyHash, Tx (Tx), TxId, TxIn (TxIn), TxInType (..), TxOut,
               TxOutRef (TxOutRef, txOutRefId, txOutRefIdx), Value, txFee, txMint, txOutValue, txOutputs, txSignatures)
import Ledger.Ada (Ada (Lovelace))
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Scripts (Datum (getDatum), Script, Validator, ValidatorHash (ValidatorHash), unValidatorScript,
                       unversioned)
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Doc, Pretty, defaultLayoutOptions, fill, indent, layoutPretty, line, parens, pretty, viaShow,
                      vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Wallet.Emulator.Folds (EmulatorEventFold)
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Types (Wallet (Wallet))
import Wallet.Rollup (doAnnotateBlockchain)
import Wallet.Rollup.Types (AnnotatedTx (AnnotatedTx), BeneficialOwner (OwnedByPaymentPubKey, OwnedByScript),
                            DereferencedInput (DereferencedInput, InputNotFound, originalInput, refersTo),
                            SequenceId (SequenceId, slotIndex, txIndex), balances, dereferencedInputs,
                            toBeneficialOwner, tx, txId, valid)

showBlockchainFold :: [(PaymentPubKeyHash, Wallet)] -> EmulatorEventFold (Either Text Text)
showBlockchainFold walletKeys =
    let r txns =
            renderStrict . layoutPretty defaultLayoutOptions
            <$> runReaderT (render txns) (Map.fromList walletKeys)
    in fmap r Folds.annotatedBlockchain

showBlockchain :: [(PaymentPubKeyHash, Wallet)] -> Blockchain -> Either Text Text
showBlockchain walletKeys blockchain =
    flip runReaderT (Map.fromList walletKeys) $ do
        annotatedBlockchain <- doAnnotateBlockchain blockchain
        doc <- render $ reverse annotatedBlockchain
        pure . renderStrict . layoutPretty defaultLayoutOptions $ doc

type RenderM = ReaderT (Map PaymentPubKeyHash Wallet) (Either Text)

class Render a where
    render :: a -> RenderM (Doc ann)

newtype RenderPretty a =
    RenderPretty a

instance Pretty a => Render (RenderPretty a) where
    render (RenderPretty a) = pure $ pretty a

instance Render [[AnnotatedTx]] where
    render blockchain =
        vsep . intersperse mempty . fold <$>
        itraverse
            (\slotIndex ->
                 itraverse
                     (\txIndex tx -> do
                          i <- render SequenceId {..}
                          v <- render tx
                          pure $ vsep ["====" <+> i <+> "====", v]))
            blockchain

instance Render AnnotatedTx where
    render AnnotatedTx { txId
                       , tx = Tx {txOutputs, txMint, txFee, txSignatures}
                       , dereferencedInputs
                       , balances
                       , valid = True
                       } =
        vsep <$>
        sequence
            [ heading "TxId:" txId
            , heading "Fee:" txFee
            , heading "Mint:" txMint
            , heading "Signatures" txSignatures
            , pure "Inputs:"
            , indent 2 <$> numbered "----" "Input" dereferencedInputs
            , pure line
            , pure "Outputs:"
            , indent 2 <$> numbered "----" "Output" txOutputs
            , pure line
            , pure "Balances Carried Forward:"
            , indented balances
            ]
    render AnnotatedTx { txId
                       , tx = Tx { txFee }
                       , valid = False
                       } =
        vsep <$>
        sequence
            [ pure "Invalid transaction"
            , heading "TxId:" txId
            , heading "Fee:" txFee
            ]

heading :: Render a => Doc ann -> a -> ReaderT (Map PaymentPubKeyHash Wallet) (Either Text) (Doc ann)
heading t x = do
    r <- indented x
    pure $ fill 10 t <> r

instance Render SequenceId where
    render SequenceId {..} =
        pure $
        "Slot #" <> viaShow slotIndex <> "," <+> "Tx #" <> viaShow txIndex

instance Render CurrencySymbol where
    render (CurrencySymbol "")    = pure "Ada"
    render (CurrencySymbol other) = render other

instance Render TokenName where
    render (TokenName "") = pure "Lovelace"
    render t              = pure $ pretty $ Value.toString t

instance Render PlutusTx.BuiltinByteString where
    render = pure . pretty . JSON.encodeByteString . PlutusTx.fromBuiltin

deriving via RenderPretty PlutusTx.Data instance
         Render PlutusTx.Data

instance Render PlutusTx.BuiltinData where
    render d = render $ PlutusTx.builtinDataToData d

deriving newtype instance Render Value

instance (Render k, Render v) => Render (AssocMap.Map k v) where
    render m
        | null (AssocMap.toList m) = pure "-"
        | otherwise =
            vsep <$>
            traverse
                (\(k, v) -> do
                     rk <- render k
                     rv <- render v
                     pure $ fill 8 (rk <> ":") <> indent 2 rv)
                (AssocMap.toList m)

instance Render (Map BeneficialOwner Value) where
    render xs
        | Map.null xs = pure "-"
        | otherwise = do
            entries <-
                traverse
                    (\(k, v) -> do
                         rk <- render k
                         rv <- render v
                         pure $ vsep [rk, "Value:", indent 2 rv])
                    (Map.toList xs)
            pure $ vsep $ intersperse mempty entries

instance Render (Map PubKey Signature) where
    render xs
        | Map.null xs = pure "-"
        | otherwise = do
            entries <-
                traverse
                    (\(k, v) -> do
                         rk <- render k
                         rv <- render v
                         pure $ vsep [rk, indent 2 rv])
                    (Map.toList xs)
            pure $ vsep $ intersperse mempty entries

deriving via RenderPretty Text instance Render Text

deriving via RenderPretty String instance Render String

deriving via RenderPretty Integer instance Render Integer

deriving via RenderPretty Address instance Render Address

instance Render Wallet where
    render (Wallet _ n) = pure $ "Wallet" <+> viaShow n

instance Render BeneficialOwner where
    render (OwnedByScript address) = ("Script:" <+>) <$> render address
    render (OwnedByPaymentPubKey pkh) = do
        walletKeys <- ask
        wallet <- lookupWallet pkh walletKeys
        w <- render wallet
        p <- render pkh
        pure $ p <+> parens w

instance Render Ada where
    render ada@(Lovelace l)
        | Ada.isZero ada = pure "-"
        | otherwise = pure (pretty l)

instance Render (Digest SHA256) where
    render = render . abbreviate 40 . JSON.encodeSerialise

deriving via RenderPretty TxId instance Render TxId

instance Render PubKey where
    render pubKey =
        pure $
        let v = Text.pack (show (pretty pubKey))
         in "PubKey:" <+> pretty (abbreviate 40 v)

instance Render PubKeyHash where
    render pkh =
        pure $
        let v = Text.pack (show (pretty pkh))
         in "PubKeyHash:" <+> pretty (abbreviate 40 v)

instance Render PaymentPubKey where
    render pubKey =
        pure $
        let v = Text.pack (show (pretty pubKey))
         in "PaymentPubKey:" <+> pretty (abbreviate 40 v)

instance Render PaymentPubKeyHash where
    render pkh =
        pure $
        let v = Text.pack (show (pretty pkh))
         in "PaymentPubKeyHash:" <+> pretty (abbreviate 40 v)

instance Render Signature where
    render sig =
        pure $
        let v = JSON.encodeSerialise sig
         in "Signature:" <+> pretty (abbreviate 40 v)

instance Render Script where
    render script =
        pure $
        let v = JSON.encodeSerialise script
         in "Script:" <+> pretty (abbreviate 40 v)

instance Render Validator where
    render = render . unValidatorScript

deriving newtype instance Render ValidatorHash

instance Render Datum where
    render = render . getDatum

instance Render a => Render (Set a) where
    render xs = vsep <$> traverse render (Set.toList xs)

instance Render DereferencedInput where
    render (InputNotFound txKey) = pure $ "Input not found:" <+> pretty txKey
    render DereferencedInput {originalInput, refersTo} =
        vsep <$>
        sequence
            [render refersTo, pure "Source:", indent 2 <$> render originalInput]

instance Render TxIn where
    render (TxIn txInRef (Just txInType)) =
        vsep <$> sequence [render txInRef, render txInType]
    render (TxIn txInRef Nothing) = render txInRef

instance Render TxInType where
    render (ConsumeScriptAddress validator _ _) = render (unversioned validator)
    render ConsumePublicKeyAddress              = pure mempty
    render ConsumeSimpleScriptAddress           = pure mempty

instance Render TxOutRef where
    render TxOutRef {txOutRefId, txOutRefIdx} =
        vsep <$>
        sequence [heading' "Tx:" txOutRefId, heading' "Output #" txOutRefIdx]
      where
        heading' t x = do
            r <- render x
            pure $ fill 8 t <> r

instance Render TxOut where
    render txOut =
        vsep <$>
        sequence
            [ mappend "Destination:" . indent 2 <$>
              render (toBeneficialOwner txOut)
            , pure "Value:"
            , indent 2 <$> render (txOutValue txOut)
            ]

------------------------------------------------------------
indented :: Render a => a -> RenderM (Doc ann)
indented x = indent 2 <$> render x

numbered :: Render a => Doc ann -> Doc ann -> [a] -> RenderM (Doc ann)
numbered separator title xs =
    vsep . intersperse mempty <$> itraverse numberedEntry xs
  where
    numberedEntry index x = do
        v <- render x
        pure $ vsep [separator <+> title <+> viaShow index <+> separator, v]

------------------------------------------------------------
lookupWallet ::
       MonadError Text m => PaymentPubKeyHash -> Map PaymentPubKeyHash Wallet -> m Wallet
lookupWallet pkh (Map.lookup pkh -> Just wallet) = pure wallet
lookupWallet pkh _ =
    throwError $
    "Could not find referenced PubKeyHash: " <> Text.pack (show pkh)

abbreviate :: Int -> Text -> Text
abbreviate n t =
    let prefix = Text.take n t
     in if prefix == t
            then t
            else prefix <> "..."

{- Note [Serialising Digests from Crypto.Hash]
This is more complicated than you might expect.  If you say
`encode = encode . BA.unpack` then the contents of the digest are
unpacked into a `Word8` list with 32 entries.  However, when cborg
serialises a list, every element in the output is preceded by a type
tag (in this case, 24), and this means that the serialised version is
about 64 bytes long, twice the length of the original data.  Packing
the `Word8` list into a `ByteString` first fixes this because cborg
just serialises it as a sequence of contiguous bytes. -}

instance Serialise (Digest SHA256) where
    encode = encode . BSS.pack . BA.unpack
    decode = do
      d :: BSS.ByteString <- decode
      let bs :: BA.Bytes = BA.pack . BSS.unpack $ d
      case digestFromByteString bs of
        Nothing -> fail $ "Couldn't decode SHA256 Digest: " ++ show d
        Just v  -> pure v

instance ToJSON (Digest SHA256) where
    toJSON = JSON.String . JSON.encodeSerialise

instance FromJSON (Digest SHA256) where
    parseJSON = JSON.decodeSerialise
