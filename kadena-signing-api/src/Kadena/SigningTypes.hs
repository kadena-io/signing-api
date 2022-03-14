{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TypeOperators #-}

module Kadena.SigningTypes where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import qualified Data.List.Split as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics

import Pact.Parse
import Pact.Types.Command
import Pact.Types.Hash
-- TODO: Rip out sig data dependency
import Pact.Types.SigData

newtype SignatureList = SignatureList  [(PublicKeyHex, Maybe UserSig)]
  deriving (Eq,Show,Generic)

instance ToJSON SignatureList where
  toJSON (SignatureList sl ) = object $
    map (\(k,ms) -> (unPublicKeyHex k, maybe Null (toJSON . _usSig) ms)) sl
    where
      k .?= v = case v of
        Nothing -> mempty
        Just v' -> [k .= v']

instance FromJSON SignatureList where
  parseJSON = withObject "SignatureList" $ \o -> do
    fmap SignatureList $ f o
    where
      f = mapM g . HM.toList
      g (k,Null) = pure (PublicKeyHex k, Nothing)
      g (k,String t) = pure (PublicKeyHex k, Just $ UserSig t)
      g (_,v) = typeMismatch "Signature should be String or Null" v

data CommandSigRequest = CommandSigRequest
  { _csr_sigs :: SignatureList
  , _csr_cmd :: Text
  } deriving (Eq,Show,Generic)

instance ToJSON CommandSigRequest where
  toJSON (CommandSigRequest s c) = object $
    [ "sigs" .= s
    , "cmd" .= c
    ]

instance FromJSON CommandSigRequest where
  parseJSON = withObject "CommandSigRequest" $ \o -> do
    s <- o .: "sigs"
    c <- o .: "cmd"
    pure $ CommandSigRequest s c

data HashSigRequest = HashSigRequest
  { _hsr_sigs :: SignatureList
  , _hsr_hash :: PactHash
  } deriving (Eq,Show,Generic)

instance ToJSON HashSigRequest where
  toJSON (HashSigRequest s h) = object $
    [ "sigs" .= s
    , "hash" .= h
    ]

instance FromJSON HashSigRequest where
  parseJSON = withObject "HashSigRequest" $ \o -> do
    s <- o .: "sigs"
    h <- o .: "hash"
    pure $ HashSigRequest s h

commandToCommandSigRequest :: Command Text -> Either String CommandSigRequest
commandToCommandSigRequest c = do
  let ep = traverse parsePact =<< (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload c)
  case ep :: Either String (Payload Value ParsedCode) of
    Left e -> Left $ "Error decoding payload: " <> e
    Right p -> do
      let sigs = map (\s -> (PublicKeyHex $ _siPubKey s, Nothing)) $ _pSigners p
      Right $ CommandSigRequest (SignatureList sigs) $ _cmdPayload c

commandSigRequestToCommand :: CommandSigRequest -> Either String (Command Text)
commandSigRequestToCommand (CommandSigRequest (SignatureList sigList) c) = do
  payload :: Payload Value ParsedCode <- traverse parsePact =<< eitherDecodeStrict' (T.encodeUtf8 c)
  let sigMap = M.fromList sigList
  -- It is ok to use a map here because we're iterating over the signers list and only using the map for lookup.
      sigs = catMaybes $ map (\signer -> join $ M.lookup (PublicKeyHex $ _siPubKey signer) sigMap) $ _pSigners payload
      h = hash (T.encodeUtf8 c)
  pure $ Command c sigs h

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
