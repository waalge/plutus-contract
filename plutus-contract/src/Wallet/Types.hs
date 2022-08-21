-- TODO stays completely (minus the mockUUIDs for testing?)
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Defines a number of types that are used in Wallet.XXX modules
module Wallet.Types(
    ContractInstanceId(..)
    , contractInstanceIDs
    , randomID
    , ContractActivityStatus(..)
    , parseContractActivityStatus
    , Notification(..)
    , NotificationError(..)
    , EndpointDescription(..)
    , EndpointValue(..)
    ) where

import           Control.Lens.TH            (makeClassyPrisms)
import           Data.Aeson                 (FromJSON, FromJSONKey, ToJSON,
                                             ToJSONKey)
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.UUID                  (UUID)
import qualified Data.UUID.Extras           as UUID
import qualified Data.UUID.V4               as UUID
import           GHC.Generics               (Generic)
import qualified Language.Haskell.TH.Syntax as TH
import           Prettyprinter              (Pretty (..), colon, hang, viaShow,
                                             vsep, (<+>))

import           Prettyprinter.Extras       (PrettyShow (..), Tagged (..))

import qualified Data.OpenApi.Schema        as OpenApi

-- | Unique ID for contract instance
newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (FromJSONKey, ToJSONKey)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)
    deriving Pretty via (PrettyShow UUID)

-- | A pure list of all 'ContractInstanceId' values. To be used in testing.
contractInstanceIDs :: [ContractInstanceId]
contractInstanceIDs = ContractInstanceId <$> UUID.mockUUIDs

randomID :: IO ContractInstanceId
randomID = ContractInstanceId <$> UUID.nextRandom

data ContractActivityStatus = Active | Stopped | Done deriving (Eq, Show, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

parseContractActivityStatus :: Text -> Maybe ContractActivityStatus
parseContractActivityStatus t = case T.toLower t of
    "active"  -> Just Active
    "stopped" -> Just Stopped
    "done"    -> Just Done
    _         -> Nothing

newtype EndpointDescription = EndpointDescription { getEndpointDescription :: String }
    deriving stock (Eq, Ord, Generic, Show, TH.Lift)
    deriving newtype (IsString, Pretty)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

newtype EndpointValue a = EndpointValue { unEndpointValue :: a }
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

deriving via (Tagged "EndpointValue:" (PrettyShow a)) instance (Show a => Pretty (EndpointValue a))

data Notification =
    Notification
        { notificationContractID       :: ContractInstanceId
        , notificationContractEndpoint :: EndpointDescription
        , notificationContractArg      :: Aeson.Value
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty Notification where
    pretty Notification{notificationContractID,notificationContractEndpoint,notificationContractArg} =
        hang 2 $ vsep
            [ "Instance:" <+> pretty notificationContractID
            , "Endpoint:" <+> pretty notificationContractEndpoint
            , "Argument:" <+> viaShow notificationContractArg
            ]

data NotificationError =
    EndpointNotAvailable ContractInstanceId EndpointDescription
    | MoreThanOneEndpointAvailable ContractInstanceId EndpointDescription
    | InstanceDoesNotExist ContractInstanceId
    | NotificationJSONDecodeError EndpointDescription Aeson.Value String
    -- ^ Indicates that the target contract does not have the expected schema
    --
    -- TODO: SCP-2137
    -- Not currently used. As endpoint parameter decoding happends inside the Contract and
    -- a throwError is used is decoding failed.
    -- However, still valuable to be used by the PAB to throw an error is an endpoint
    -- could not be decoded.
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty NotificationError where
    pretty = \case
        EndpointNotAvailable i ep -> "Endpoint" <+> pretty ep <+> "not available on" <+> pretty i
        MoreThanOneEndpointAvailable i ep -> "Endpoint" <+> pretty ep <+> "is exposed more than once on" <+> pretty i
        InstanceDoesNotExist i -> "Instance does not exist:" <+> pretty i
        NotificationJSONDecodeError ep vv e ->
                "Notification JSON decoding error:"
                    <+> pretty e
                    <> colon
                    <+> pretty (BSL8.unpack (JSON.encodePretty vv))
                    <+> pretty ep

makeClassyPrisms ''NotificationError
