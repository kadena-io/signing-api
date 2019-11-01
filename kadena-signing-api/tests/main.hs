{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (when)
import Data.Either (isLeft, isRight)
import qualified Data.Char as Char
import qualified Data.Text as T

import Kadena.SigningApi (mkAccountName,isValidAccountNameCharacter)

prop_accountname_valid :: Property
prop_accountname_valid = property $ do
  nom <- forAll $ Gen.text (Range.linear 0 300) Gen.unicode
  let acc = mkAccountName nom
      len = T.length nom
      anyInvalid = not $ T.foldl' (\b c -> b && isValidAccountNameCharacter c) True nom

  classify "valid length" (len >= 3 && len <= 256)
  classify "invalid length" (len < 3 || len > 256)
  classify "valid chars" (not anyInvalid)
  classify "invalid chars" anyInvalid

  if isRight acc then
    if len < 3 then annotate "Minimum length < 3" *> failure
    else if len > 256 then annotate "Maximum length > 256" *> failure
    else if anyInvalid then annotate "Invalid characters detected" *> failure
    else success -- We've created an AccountName that meets the requirements
    else success -- We've avoided creating an invalid AccountName

main :: IO Bool
main = checkParallel $$(discover)
