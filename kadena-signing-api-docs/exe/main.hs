{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Types.Command
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

dummyCommandSigData :: CommandSigData
dummyCommandSigData = CommandSigData
  (SignatureList [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1", Nothing)])
  "<cmd here>"

instance ToSchema CommandSigData where
  declareNamedSchema _ =
    swaggerDescription "the signature data for a command" $
      namedSchema "CommandSigData" $ sketchSchema $
        dummyCommandSigData

-- instance ToSchema HashSigData where
--   declareNamedSchema _ =
--     swaggerDescription "the signature data for a hash" $
--       namedSchema "HashSigData" $ sketchSchema $
--         HashSigData
--           (SignatureList [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1", Nothing)])
--           "<hash here>"

instance ToSchema QuickSignRequest where
  declareNamedSchema _ =
    swaggerDescription "completed transaction bytes to be signed" $
      namedSchema "QuickSignRequest" $ sketchSchema $
        QuickSignRequest [ dummyCommandSigData ]

instance ToSchema QuickSignResponse where
  declareNamedSchema _ =
    swaggerDescription "list of SigData" $
      namedSchema "QuickSignResponse" $ sketchSchema $
        QuickSignResponse [ CommandSigData sigLst "<cmd here>" ]

        where
          sigLst =
            SignatureList [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1",
                             Just $ UserSig
                                "e103338c324190c0e86f06e2fdcc886df42562c5d74a2216c8b2cc729d255686ec5488693569da6afc57a02af5e4ec5bd013c24b4fcddd94cc94eb412e88a20d"
                            )
                          ]

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
