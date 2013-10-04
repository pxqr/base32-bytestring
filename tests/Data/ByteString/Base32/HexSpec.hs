{-# OPTIONS -fno-warn-orphans #-}
module Data.ByteString.Base32.HexSpec ( spec ) where

import Control.Applicative
import Control.Exception
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Base32.Hex
import Data.Char
import Test.Hspec
import Test.QuickCheck


instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

spec :: Spec
spec = do
  describe "encode" $ do
    it "conform rfc examples" $ do
      encode ""         `shouldBe` ""
      encode "f"        `shouldBe` "CO======"
      encode "fo"       `shouldBe` "CPNG===="
      encode "foo"      `shouldBe` "CPNMU==="
      encode "foob"     `shouldBe` "CPNMUOG="
      encode "fooba"    `shouldBe` "CPNMUOJ1"
      encode "foobar"   `shouldBe` "CPNMUOJ1E8======"

  describe "decode" $ do
    it "conform rfc examples" $ do
      decode ""         `shouldBe` ""
      decode "CO======" `shouldBe` "f"
      decode "CPNG====" `shouldBe` "fo"
      decode "CPNMU===" `shouldBe` "foo"
      decode "CPNMUOG=" `shouldBe` "foob"
      decode "CPNMUOJ1" `shouldBe` "fooba"
      decode "CPNMUOJ1E8======" `shouldBe` "foobar"

    it "inverse for encode" $ property $ \bs ->
      decode (encode bs) == bs

    it "case insensitive" $ property $ \bs ->
      decode (BC.map toLower (encode bs)) == bs

    it "fail gracefully if encoded data contains non alphabet chars" $ do
      evaluate (decode "#=======")         `shouldThrow` anyErrorCall
      evaluate (decode "AAAAAAAA#=======") `shouldThrow` anyErrorCall

  describe "decodeLenient" $ do
    it "conform RFC examples" $ do
      decode ""         `shouldBe` ""
      decode "CO======" `shouldBe` "f"
      decode "CPNG====" `shouldBe` "fo"
      decode "CPNMU===" `shouldBe` "foo"
      decode "CPNMUOG=" `shouldBe` "foob"
      decode "CPNMUOJ1" `shouldBe` "fooba"
      decode "CPNMUOJ1E8======" `shouldBe` "foobar"

    it "inverse for encode" $ property $ \bs ->
      decodeLenient (encode bs) == bs

    it "case insensitive" $ property $ \bs ->
      decodeLenient (BC.map toLower (encode bs)) == bs

    it "skip non alphabet chars" $ do
      decodeLenient "|"   `shouldBe` ""
      decodeLenient "C|O" `shouldBe` "f"
