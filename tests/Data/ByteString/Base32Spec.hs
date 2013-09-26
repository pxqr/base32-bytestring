{-# OPTIONS -fno-warn-orphans #-}
module Data.ByteString.Base32Spec (spec) where

import Control.Applicative
import Data.ByteString as BS
import Data.ByteString.Internal as BS
import Data.ByteString.Base32 as Base32
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

    it "padding less than" $ property $ \bs ->
      count (c2w '=') bs `shouldSatisfy` (< 8)

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

--  describe "decodeLenient" $ do
