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
import PactSwagger
import Servant.Swagger
import Kadena.SigningApi
import Kadena.SigningTypes

instance ToSchema AccountName where
  declareNamedSchema = swaggerDescription desc .
                       declareGenericString
    where
      desc = "The name of an account in the coin contract. In the SigningRequest sender field, this will be the account used to pay the transaction's gas price."

instance ToSchema DappCap where
  declareNamedSchema = (swaggerDescription "a capability required by the transaction with amplifying information to help the user") .
                       lensyDeclareNamedSchema 10

instance ToSchema SigningRequest where
  declareNamedSchema = (swaggerDescription "transaction information sent to the wallet for signing") .
                       lensyDeclareNamedSchema 16

instance ToSchema SigningResponse where
  declareNamedSchema = (swaggerDescription "wallet response that includes the signed transaction") .
                       lensyDeclareNamedSchema 17

instance ToSchema CommandSigRequest where
  declareNamedSchema _ =
    swaggerDescription "the signature data for a command" $
      namedSchema "CommandSigRequest" $ sketchSchema $
        CommandSigRequest
          (SignatureList [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1", Nothing)])
          "<cmd here>"

instance ToSchema QuickSignRequest where
  declareNamedSchema = (swaggerDescription "completed transaction bytes to be signed") .
                       lensyDeclareNamedSchema 11

instance ToSchema QuickSignResponse where
  declareNamedSchema = (swaggerDescription "list of SigData") .
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
