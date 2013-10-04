{-# OPTIONS -fno-warn-orphans #-}
module Data.ByteString.Base32Spec (spec) where

import Control.Applicative
import Control.Exception
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Base32 as Base32
import Data.Char
import Test.Hspec
import Test.QuickCheck


instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

spec :: Spec
spec = do
  describe "encode" $ do
    it "conform RFC examples" $ do
      encode ""       `shouldBe` ""
      encode "f"      `shouldBe` "MY======"
      encode "fo"     `shouldBe` "MZXQ===="
      encode "foo"    `shouldBe` "MZXW6==="
      encode "foob"   `shouldBe` "MZXW6YQ="
      encode "fooba"  `shouldBe` "MZXW6YTB"
      encode "foobar" `shouldBe` "MZXW6YTBOI======"

    it "size always multiple of 8 bytes" $ property $ \bs ->
      (BS.length (encode bs) `rem` 8) `shouldBe` 0

    it "padding less than 8 bytes" $ property $ \bs ->
      BC.count '=' bs `shouldSatisfy` (< 8)

  describe "decode" $ do
    it "conform RFC examples" $ do
      decode ""                 `shouldBe` ""
      decode "MY======"         `shouldBe` "f"
      decode "MZXQ===="         `shouldBe` "fo"
      decode  "MZXW6==="        `shouldBe` "foo"
      decode "MZXW6YQ="         `shouldBe` "foob"
      decode "MZXW6YTB"         `shouldBe` "fooba"
      decode "MZXW6YTBOI======" `shouldBe` "foobar"

    it "inverse for encode" $ property $ \bs ->
      decode (encode bs) == bs

    it "case insensitive" $ property $ \bs ->
      decode (BC.map toLower (encode bs)) == bs

    it "fail gracefully if encoded data contains non alphabet chars" $ do
      evaluate (decode "0=======")         `shouldThrow` anyErrorCall
      evaluate (decode "AAAAAAAA0=======") `shouldThrow` anyErrorCall

  describe "decodeLenient" $ do
    it "conform RFC examples" $ do
      decodeLenient ""                 `shouldBe` ""
      decodeLenient "MY======"         `shouldBe` "f"
      decodeLenient "MZXQ===="         `shouldBe` "fo"
      decodeLenient  "MZXW6==="        `shouldBe` "foo"
      decodeLenient "MZXW6YQ="         `shouldBe` "foob"
      decodeLenient "MZXW6YTB"         `shouldBe` "fooba"
      decodeLenient "MZXW6YTBOI======" `shouldBe` "foobar"

    it "inverse for encode" $ property $ \bs ->
      decodeLenient (encode bs) == bs

    it "case insensitive" $ property $ \bs ->
      decodeLenient (BC.map toLower (encode bs)) == bs

    it "skip non alphabet chars" $ do
      decodeLenient "|"   `shouldBe` ""
      decodeLenient "M|Y" `shouldBe` "f"
