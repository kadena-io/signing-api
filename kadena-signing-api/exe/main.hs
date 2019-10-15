{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import Kadena.SigningApi

main :: IO ()
main = do
  BS.putStrLn $ BSL.toStrict $ encode signingSwagger
