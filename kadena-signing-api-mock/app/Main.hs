{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad

import qualified Data.ByteString as BS
import Data.Default.Class
import Data.Proxy
import Data.Reflection

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
import Pact.Types.Util

instance Arbitrary SigningResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arg (Command a) a, Arbitrary a) => Arbitrary (Command a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Reifies h HashAlgo => Arbitrary (TypedHash h) where
  arbitrary = hash <$> arbitrary
  -- There's no meaningful way to shrink a hash
  shrink = const []

instance Arbitrary UserSig where
  arbitrary = UserSig . toB16Text <$> arbitrary
  -- We could in theory come from B16 shrink and the go back to B16, but
  -- don't think it's worth it
  shrink = const []

instance Arbitrary PublicKeyHex where
  arbitrary = PublicKeyHex . toB16Text . BS.pack <$> replicateM 32 arbitrary
  -- There's no meaningful way to shrink a hash
  shrink = const []

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
