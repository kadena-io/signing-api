{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Kadena.SigningApi where

import Control.Lens
import Data.Aeson
import qualified Data.Aeson as A
import Data.Kind (Type)
import qualified Data.List.Split as L
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char
import GHC.Generics
import Pact.Server.API
import Pact.Types.Capability (SigCapability(..))
import Pact.Types.ChainMeta (TTLSeconds(..))
import Pact.Types.Runtime (GasLimit(..), ChainId, PublicKey)
import Pact.Types.Command (Command)
import Trasa.Core
import qualified Trasa.Method as M


newtype AccountName = AccountName
  { unAccountName :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Smart constructor for account names. The only restriction in the coin
-- contract (as it stands) appears to be that accounts can't be an empty string
mkAccountName :: Text -> Either Text AccountName
mkAccountName n =
  if not (isValidCharset n) then Left "Invalid Character detected. Must be Latin1, no spaces, control characters or '|'"
  else if not (isCorrectSize n) then Left "Incorrect length. Must be between 3 and 256 characters in length."
  else Right $ AccountName n

isCorrectSize :: Text -> Bool
isCorrectSize n = let l = T.length n in l >= 3 && l <= 256

isValidCharset :: Text -> Bool
isValidCharset = T.all isValidAccountNameCharacter

isValidAccountNameCharacter :: Char -> Bool
isValidAccountNameCharacter char = Char.isLatin1 char
  && not ( Char.isControl char ||
           char == '\NUL'
         )

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

-- | Aeson encoding options for compact encoding.
--
--   We pass on the most compact sumEncoding as it could be unsound for certain types.
--
--   But we assume the following naming of constructor names (sum typs) and
--   field names (records): _TypeName_Blah and _typename_blah.
--
--   In particular we assume that only the string after the last underscore is
--   significant for distinguishing field names/constructor names. If this
--   assumption is not met this encoding might not result in the same decoding.
compactEncoding :: Options
compactEncoding = defaultOptions
    { A.fieldLabelModifier = shortener
    , A.allNullaryToStringTag = True
    , A.constructorTagModifier = shortener
    , A.omitNothingFields = True
    , A.sumEncoding = ObjectWithSingleField
    , A.unwrapUnaryRecords = True
    , A.tagSingleConstructors = False
    }
  where
    -- As long as names are not empty or just underscores this head should be fine:
    shortener = head . reverse . filter (/= "") . L.splitOn "_"

--type SigningApi = "v1" :> V1SigningApi
--type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse
--
--signingAPI :: Proxy SigningApi
--signingAPI = Proxy

data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  SignR :: Route
    '[]
    '[]
    (Body SigningRequest)
    SigningResponse

meta :: Route caps qrys req resp -> MetaCodec caps qrys req resp
meta = \case
  SignR -> Meta
    (match "v1" ./ match "sign" ./ end)
    qend
    (body $ one bodySigningRequest)
    (resp $ one bodySigningResponse)
    M.post

bodyAeson :: (ToJSON a, FromJSON a) => BodyCodec a
bodyAeson = BodyCodec (pure "*/*") encode (bimap T.pack id . eitherDecode)

bodySigningRequest :: BodyCodec SigningRequest
bodySigningRequest = bodyAeson

bodySigningResponse :: BodyCodec SigningResponse
bodySigningResponse = bodyAeson

