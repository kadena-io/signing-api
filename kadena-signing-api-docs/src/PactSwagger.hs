{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of this file has been ripped out from an older version of pact

-- |
-- Module      :  Pact.Types.Swagger
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Swagger specification utilities and exports.
--

module PactSwagger
  (
    -- | Schemas
    byteBase64url
    -- | ToSchema implementations
  , declareGenericEmpty
  , declareGenericSchema
  , declareGenericString
  , lensyDeclareNamedSchema
  , namedSchema
    -- | Modifiers
  , fixedLength
  , toNiceString
  , optionConstructor
  , optionFieldLabel
  , optionsOf
  , swaggerProperties
  , swaggerDescription
  , swaggerType
  , schemaOf
  , withSchema
    -- | Re-exports
  , ToSchema(..)
    -- | Debugging
  , debugSchema
  ) where

import GHC.Exts (fromList)
import Data.Decimal
import Data.Aeson (toJSON, ToJSON,Value)
import Data.Proxy (Proxy(..))
import Data.Swagger hiding (Info)
import Data.Swagger.Declare
import Data.Swagger.Internal hiding (Info)
import Data.Swagger.Internal.TypeShape
import Data.Swagger.Internal.Schema
import GHC.Generics
import Control.Lens (set)
import Data.Text as T
import Data.Default (def)
import Pact.Time

import qualified Pact.Analyze.Remote.Types as Analyze
import Pact.Parse
import Pact.Types.API
import Pact.Types.Capability
import Pact.Types.ChainId
import Pact.Types.ChainMeta
import Pact.Types.Codec (timeCodec, encoder)
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Exp
import Pact.Types.Gas
import Pact.Types.Hash
import Pact.Types.Info (Info)
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.Persistence (TxId)
import Pact.Types.Pretty
import Pact.Types.SigData
import Pact.Types.Runtime (PactError,PactErrorType,StackFrame,PactEvent, OutputType(..))
import Pact.Types.Term
import Pact.Types.Util



#if MIN_VERSION_swagger2(2,4,0)
just :: a -> Maybe a
just = Just
#else
just :: a -> a
just = id
#endif

-- | 'ToSchema' generic implementation with provided schema.
declareGenericSchema ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  Schema -> proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericSchema s _ = pure $
  genericNameSchema defaultSchemaOptions (Proxy :: Proxy a) s

-- | 'ToSchema' generic implementation with empty schema.
declareGenericEmpty ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericEmpty = declareGenericSchema mempty

declareGenericString ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericString = declareGenericSchema (schemaOf $ swaggerType SwaggerString)

-- | 'ToSchema' instance for unprefixing single-constructor field names.
lensyDeclareNamedSchema ::
  forall a proxy.
  (GenericHasSimpleShape a
    "genericDeclareNamedSchemaUnrestricted"
    (GenericShape (Rep a)),
   Generic a, GToSchema (Rep a))
  => Int
  -> proxy a
  -> Declare (Definitions Schema) NamedSchema
lensyDeclareNamedSchema i _ =
  genericDeclareNamedSchema (fromAesonOptions $ lensyOptions i) (Proxy :: Proxy a)


namedSchema :: Text -> Schema -> Declare (Definitions Schema) NamedSchema
namedSchema n s = return $ NamedSchema (Just n) s


-- | like 'byteSchema' but with (non-standard) "base64url" in format.
byteBase64url :: Schema
byteBase64url = set type_ (just SwaggerString) . set format (Just "base64url") $ mempty

fixedLength :: Integral i => i -> Schema -> Schema
fixedLength i =
  set maxLength (Just $ fromIntegral i) .
  set minLength (Just $ fromIntegral i)

toNiceString :: Int -> String -> String
toNiceString i = unpack . T.toLower . T.drop i . pack


optionConstructor :: (String -> String) -> SchemaOptions -> SchemaOptions
optionConstructor f o = o { constructorTagModifier = f }

optionFieldLabel :: (String -> String) -> SchemaOptions -> SchemaOptions
optionFieldLabel f o = o { fieldLabelModifier = f }

optionsOf :: (SchemaOptions -> SchemaOptions) -> SchemaOptions
optionsOf f = f defaultSchemaOptions


swaggerProperties :: [(Text, Referenced Schema)] -> Schema -> Schema
swaggerProperties p = set properties (fromList p)

swaggerDescription :: Text
  -> Declare (Definitions Schema) NamedSchema
  -> Declare (Definitions Schema) NamedSchema
swaggerDescription d s = do
  namedSchema' <- s
  let schema' = set description (Just d) (_namedSchemaSchema namedSchema')
  return $ namedSchema' { _namedSchemaSchema = schema' }

swaggerType :: SwaggerType 'SwaggerKindSchema -> Schema -> Schema
swaggerType = set type_ . just

schemaOf :: (Schema -> Schema) -> Schema
schemaOf f = f mempty

withSchema :: Schema -> (Schema -> Schema) -> Schema
withSchema s f = f s

-- debugSchema
debugSchema :: forall a proxy. ToSchema a => proxy a -> NamedSchema
debugSchema _ = undeclare $ declareNamedSchema (Proxy :: Proxy a)
-- | Swagger ORPHANS

instance (ToSchema a) => ToSchema (Command a) where
  declareNamedSchema =
    (swaggerDescription "transaction command with stringified JSON payload (cmd)") .
    (genericDeclareNamedSchema $
      optionsOf $ optionFieldLabel transform)
    where transform n = case n of
            "_cmdPayload" -> "cmd"
            _ -> lensyConstructorToNiceJson 4 n

instance ToSchema UserSig where
  declareNamedSchema =
    (swaggerDescription "crypto signature by secret key of command payload") .
    (lensyDeclareNamedSchema 3)

pactHashSchema :: Schema
pactHashSchema = withSchema byteBase64url $ fixedLength pactHashLength

instance ToSchema (TypedHash 'Blake2b_256) where
  declareNamedSchema = (swaggerDescription "blake2 hash in base64 of command payload") .
    (declareGenericSchema pactHashSchema)

instance ToSchema RequestKeys where
  declareNamedSchema = lensyDeclareNamedSchema 3

instance ToSchema RequestKey where
  declareNamedSchema = (swaggerDescription "command's request key (i.e. the hash of command payload)") .
    (declareGenericSchema pactHashSchema)

instance (ToSchema a) => ToSchema (CommandResult a) where
  declareNamedSchema = (swaggerDescription "result of attempting to execute a pact command") .
    (lensyDeclareNamedSchema 3)

instance ToSchema PactEvent where
  declareNamedSchema = (swaggerDescription "events emitted in transaction") .
      (genericDeclareNamedSchema defaultSchemaOptions)

instance ToSchema TxId where
  declareNamedSchema = (swaggerDescription "command's transaction id") .
    (genericDeclareNamedSchema defaultSchemaOptions)
instance ToSchema Gas where
  declareNamedSchema = (swaggerDescription "gas consummed by command") .
    (genericDeclareNamedSchema defaultSchemaOptions)
instance ToSchema Hash where
  declareNamedSchema = (swaggerDescription "the hash of the pact execution's logs") .
    (declareGenericSchema pactHashSchema)
instance ToSchema PactExec where
  declareNamedSchema = (swaggerDescription "output of a Continuation if one occurred in the command.") .
    (lensyDeclareNamedSchema 3)
instance ToSchema PactId
instance ToSchema PactContinuation where
   declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema Name where
  declareNamedSchema = declareGenericString

instance ToSchema Value where
  declareNamedSchema = (swaggerDescription "Platform-specific data") .
    (declareGenericSchema $
      (schemaOf $ swaggerType SwaggerObject))


-- Source: https://github.com/haskell-servant/servant-swagger/issues/80
data PactResultStatus = Success | Failure
  deriving (Generic)
instance ToSchema PactResultStatus
instance ToSchema PactResult where
    declareNamedSchema _ = do
      pactErrRef <- declareSchemaRef (Proxy :: Proxy PactError)
      pactValRef <- declareSchemaRef (Proxy :: Proxy PactValue)
      statusRef <- declareSchemaRef (Proxy :: Proxy PactResultStatus)
      let p = [ ("status",statusRef), ("error", pactErrRef),("data" , pactValRef) ]
          ns = namedSchema "PactResult"
               (schemaOf $ swaggerType SwaggerObject .
                           swaggerProperties p .
                           set minProperties (Just 2) .
                           set maxProperties (Just 2)
               )
      swaggerDescription "either a pact error or the last pact expression output as a pact value" ns

instance ToSchema PactError where
  declareNamedSchema = genericDeclareNamedSchema $
    optionsOf $ optionFieldLabel transform
    where transform n = case n of
            "peDoc" -> "message"
            _ -> lensyConstructorToNiceJson 2 n
instance ToSchema PactErrorType
instance ToSchema Info where
  declareNamedSchema = declareGenericString
instance ToSchema StackFrame where
  declareNamedSchema = declareGenericString
instance ToSchema Doc where
  declareNamedSchema = declareGenericString

instance ToSchema ChainId where
  declareNamedSchema = swaggerDescription "chainweb chain ID where the transaction will be executed" .
                       declareGenericString
instance ToSchema ModuleHash where
  declareNamedSchema = declareGenericSchema pactHashSchema
instance ToSchema Provenance where
   declareNamedSchema = lensyDeclareNamedSchema 2
instance ToSchema Yield where
   declareNamedSchema = lensyDeclareNamedSchema 2

instance ToSchema PactValue where
  declareNamedSchema = (swaggerDescription "data from Pact execution represented as JSON") .
    (genericDeclareNamedSchema $
      optionsOf $ optionConstructor $ toNiceString 1)

instance ToSchema ModRef where
  declareNamedSchema = lensyDeclareNamedSchema 4

instance ToSchema Literal where
  declareNamedSchema = genericDeclareNamedSchema $
    (optionsOf $ optionConstructor $ toNiceString 1)
      { unwrapUnaryRecords = True }

instance ToSchema UTCTime where
  declareNamedSchema _ = namedSchema "UTCTime" $ sketchSchema $
    posixEpoch -- IS THIS OK?

instance ToSchema Decimal where
  declareNamedSchema _ = return $
    NamedSchema (Just "Decimal")
                (schemaOf $ swaggerType SwaggerNumber)


-- Adapted from 'Map k v' as a naive instance will cause an infinite loop!!
-- 2.2 swagger2 compat means not using 'additionalProperties' for now
instance ToSchema (ObjectMap PactValue) where
  declareNamedSchema _ = do
    -- Swagger 2.2 compat, not doing schema ref for pact value
    sref <- declareSchemaRef (Proxy :: Proxy PactValue)
    namedSchema "ObjectMap"
      (schemaOf $
        swaggerType SwaggerObject .
        set additionalProperties (Just $ AdditionalPropertiesSchema sref)
      )

instance ToSchema (Guard PactValue) where
  declareNamedSchema = genericDeclareNamedSchema $
    optionsOf $ optionConstructor $ toNiceString 1

instance ToSchema PactGuard where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema KeySet where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema PublicKey where
  declareNamedSchema = declareGenericString
instance ToSchema KeySetName
instance ToSchema ModuleGuard where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema ModuleName where
   declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema NamespaceName
instance ToSchema (Object a) where
  declareNamedSchema = declareGenericSchema $
    (schemaOf $ swaggerType SwaggerObject)
instance ToSchema (UserGuard PactValue) where
  declareNamedSchema = lensyDeclareNamedSchema 3


instance ToSchema ListenerRequest where
  declareNamedSchema = lensyDeclareNamedSchema 3
instance ToSchema ListenResponse where
  declareNamedSchema = genericDeclareNamedSchema $
    optionsOf $ optionConstructor transform
    where transform n = case n of
            "ListenTimeout" -> "timeout-micros"
            _ -> toNiceString 6 n


instance ToSchema Analyze.Request where
  declareNamedSchema _ = do
    modulesRef <- declareSchemaRef (Proxy :: Proxy [ModuleDef Name])
    verifyRef <- declareSchemaRef (Proxy :: Proxy ModuleName)
    let reqSchema = schemaOf $
                    swaggerType SwaggerObject .
                    swaggerProperties [("modules",modulesRef), ("verify",verifyRef)]
    namedSchema "AnalyzeRequest" reqSchema

instance ToSchema Analyze.Response where
  declareNamedSchema _ = namedSchema "AnalyzeResponse" $ sketchSchema $
    Analyze.Response [RenderedOutput "Dummy Response" def OutputFailure]

instance ToSchema (ModuleDef t) where
  declareNamedSchema = declareGenericSchema $
    (schemaOf $ swaggerType SwaggerObject)

instance ToSchema TTLSeconds where
  declareNamedSchema = swaggerDescription desc . declareGenericSchema (schemaOf $ swaggerType SwaggerNumber)
    where
      desc = "number of seconds the transaction can wait in the mempool before expiring"
instance ToSchema ParsedInteger
instance ToSchema QualifiedName where
  declareNamedSchema = declareGenericString
instance ToSchema GasLimit where
  declareNamedSchema = swaggerDescription desc . declareGenericSchema (schemaOf $ swaggerType SwaggerNumber)
    where
      desc = "max number of gas units you want to spend on this transaction"

sigCapExample :: SigCapability
sigCapExample = SigCapability qn [a "arg1", a "arg2"]
  where
    a = PLiteral . LString
    qn = QualifiedName m "bar" def
    m = ModuleName "foo" Nothing

instance ToSchema SigCapability where
  declareNamedSchema _ =
    swaggerDescription "a capability and any arguments it requires" $
      namedSchema "SigCapability" $ sketchSchema sigCapExample

instance ToSchema (SigData Text) where
  declareNamedSchema _ =
    swaggerDescription "the signature data for a signed command" $
      namedSchema "SigData" $ sketchSchema sampleSigData
