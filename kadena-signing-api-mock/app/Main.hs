{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Default.Class
import Data.Proxy

import Servant.Mock
import Servant.Server
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic (Arg, genericArbitrary, genericShrink)

import Kadena.SigningApi
import Kadena.SigningTypes
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.SigData

instance Arbitrary SigningResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arg (Command a) a, Arbitrary a) => Arbitrary (Command a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (TypedHash h) where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving newtype instance Arbitrary UserSig
deriving newtype instance Arbitrary PublicKeyHex
deriving newtype instance Arbitrary SignatureList

instance Arbitrary CSDSigner where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary QuickSignResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CSDResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary QuicksignError where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CommandSigData where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SigningOutcome where
  arbitrary = genericArbitrary
  shrink = genericShrink


main :: IO ()
main = do
  putStrLn "Starting Mock Server at http://localhost:8080"
  loggerMW <- mkRequestLogger def
  run 8080 $ loggerMW $ serve signingAPI (mock signingAPI Proxy)
