{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Kadena.SigningApi where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Pact.Server.API
import Pact.Types.Capability (SigCapability(..))
import Pact.Types.ChainMeta (TTLSeconds(..))
import Pact.Types.Runtime (GasLimit(..), ChainId, PublicKey)
import Pact.Types.Command (Command)
import Pact.Types.SigData
import Servant.API

import Kadena.SigningTypes

-- | Values of this type are supplied by the dapp author to the wallet so the
-- wallet knows what capabilities need to be granted for the transaction.
data DappCap = DappCap
  { _dappCap_role :: Text
  -- ^ Short name for this capability that is meaningful to the user
  , _dappCap_description :: Text
  -- ^ More detailed information that the user might need to know
  , _dappCap_cap :: SigCapability
  -- ^ The actual capability
  } deriving (Eq,Ord,Show,Generic)

instance ToJSON DappCap where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON DappCap where
  parseJSON = genericParseJSON compactEncoding

data SigningRequest = SigningRequest
  { _signingRequest_code :: Text
  , _signingRequest_data :: Maybe Object
  , _signingRequest_caps :: [DappCap]
  , _signingRequest_nonce :: Maybe Text
  , _signingRequest_chainId :: Maybe ChainId
  , _signingRequest_gasLimit :: Maybe GasLimit
  , _signingRequest_ttl :: Maybe TTLSeconds
  , _signingRequest_sender :: Maybe AccountName
  , _signingRequest_extraSigners :: Maybe [PublicKey]
  } deriving (Show, Generic)

instance ToJSON SigningRequest where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON SigningRequest where
  parseJSON = genericParseJSON compactEncoding

data SigningResponse = SigningResponse
  { _signingResponse_body :: Command Text
  , _signingResponse_chainId :: ChainId
  } deriving (Eq, Show, Generic)

instance ToJSON SigningResponse where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON SigningResponse where
  parseJSON = genericParseJSON compactEncoding

newtype QuickSignRequest = QuickSignRequest
  { _quickSignRequest_commands :: [CommandSigData]
  } deriving (Eq,Generic)

instance ToJSON QuickSignRequest where
  toJSON a = object
    [ "cmds" .= _quickSignRequest_commands a
    ]

instance FromJSON QuickSignRequest where
  parseJSON = withObject "QuickSignRequest" $ \o -> do
    cmd <- o .: "reqs"
    pure $ QuickSignRequest cmd

newtype QuickSignResponse =
  QuickSignResponse { unQuickSignResponse :: [ CommandSigData ]}
  deriving (Eq,Generic)

instance ToJSON QuickSignResponse where
  toJSON a = object [ "results" .= unQuickSignResponse a ]

instance FromJSON QuickSignResponse where
  parseJSON = withObject "QuickSignResponse" $ \o -> do
    results <- o .: "results"
    pure $ QuickSignResponse results

type SigningApi = "v1" :> V1SigningApi
type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse
               :<|> "quickSign" :> ReqBody '[JSON] QuickSignRequest :> Post '[JSON] QuickSignResponse

signingAPI :: Proxy SigningApi
signingAPI = Proxy

