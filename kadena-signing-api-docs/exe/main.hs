{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Types.Util
import Pact.Types.Command
import PactSwagger
import Servant.Swagger
import Kadena.SigningApi
import Kadena.SigningTypes
import Pact.Types.Hash
import Pact.Types.SigData

instance ToSchema SigningRequest where
  declareNamedSchema = (swaggerDescription "transaction information sent to the wallet for signing") .
                       lensyDeclareNamedSchema 16

instance ToSchema SigningResponse where
  declareNamedSchema = (swaggerDescription "wallet response that includes the signed transaction") .
                       lensyDeclareNamedSchema 17

instance ToSchema AccountName where
  declareNamedSchema = swaggerDescription desc .
                       declareGenericString
    where
      desc = "The name of an account in the coin contract. In the SigningRequest sender field, this will be the account used to pay the transaction's gas price."

instance ToSchema DappCap where
  declareNamedSchema = (swaggerDescription "a capability required by the transaction with amplifying information to help the user") .
                       lensyDeclareNamedSchema 9

instance ToSchema CommandSigData where
  declareNamedSchema _ =
    swaggerDescription "the signature data for a command" $
      namedSchema "CommandSigData" $ sketchSchema $
        let (cmd, _, sl) = dummyCmdHashSL
         in CommandSigData sl cmd

dummyCmdHashSL :: (Text, Text, SignatureList)
dummyCmdHashSL = (dummyCmd, dummyHash, dummySL)
  where
    dummyHash = "LstLd5VfY5tex9SYXPxrhr3qTLovDdiWbEfpEPsLu8s"
    dummySL =  SignatureList
      [ (PublicKeyHex "ae18efd16cbd49e9a92552a2589ac01491b486fbcbb1e3f07980f945597e2033",
           Just $ UserSig "a8b9ea0fbe6b8f59917908bbd2ec473af4ba30c62e2cbab9981f7f5f62941cbb79aee832efc38675fac42a658192ba7387bbbc61017b2ca9b2f2115bc1f4c503")
      , (PublicKeyHex "fa781bdd858cd2380b5e2b654e58035f7189a6e8158686a1bb7eabb585a56e7f", Nothing)
      ]
    dummyCmd = "{\"networkId\":\"testnet04\",\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"signers\":[{\"pubKey\":\"ae18efd16cbd49e9a92552a2589ac01491b486fbcbb1e3f07980f945597e2033\",\"clist\":[{\"args\":[],\"name\":\"coin.GAS\"}]},{\"pubKey\":\"fa781bdd858cd2380b5e2b654e58035f7189a6e8158686a1bb7eabb585a56e7f\"}],\"meta\":{\"creationTime\":1663085494,\"ttl\":1800,\"gasLimit\":20,\"chainId\":\"0\",\"gasPrice\":1.0e-6,\"sender\":\"ae18efd16cbd49e9a92552a2589ac01491b486fbcbb1e3f07980f945597e2033\"},\"nonce\":\"2022-09-13 16:11:34.678231 UTC\"}"

-- instance ToSchema HashSigData where
--   declareNamedSchema _ =
--     swaggerDescription "the signature data for a hash" $
--       namedSchema "HashSigData" $ sketchSchema $
--         let (_, hash, sl) = dummyCmdHashSL
--          in HashSigData sl $ hash

instance ToSchema QuickSignRequest where
  declareNamedSchema = (swaggerDescription "completed transaction bytes to be signed") .
                       lensyDeclareNamedSchema 11

instance ToSchema QuickSignResponse where
  declareNamedSchema = (swaggerDescription "list of SigData") .
                       lensyDeclareNamedSchema 11

instance ToSchema PublicKeyHex where
  declareNamedSchema = declareGenericString

instance ToSchema SignatureList where
  declareNamedSchema = (swaggerDescription "ordered list of signers and possible signatures") .
                       lensyDeclareNamedSchema 11

signingSwagger :: Swagger
signingSwagger = toSwagger signingAPI
  & info.title .~ "Kadena Wallet Signing API"
  & info.version .~ "1.0"
  & info.description ?~ apiDesc
  & info.license ?~ "BSD3"
  & info.contact ?~ Contact (Just "Kadena LLC") (Just $ URL "https://kadena.io") (Just "info@kadena.io")
  & host ?~ Host "localhost" (Just 9467)

apiDesc :: Text
apiDesc = T.unlines
  [ "This API facilitates communication between dapps and wallets. This frees dapp developers from the complexity of managing private keys, allowing them to focus on the functionality and business logic of the application."
  , "Whenever the dapp needs to send a signed transaction, all you have to do is make an AJAX request to this API on localhost port 9467 and the user's wallet app will handle all the details of transaction signing for you."
  ]

main :: IO ()
main = do
  BS.putStrLn $ BSL.toStrict $ encode signingSwagger
