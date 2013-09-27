-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Efficient encoding and decoding of base32 encoded bytestring
--   according to RFC 4648. <http://tools.ietf.org/html/rfc4648>
--
--   This module recommended to be imported as
--   @import Data.ByteString.Base32 as Base32@ to avoid name clashes
--   with @Data.Binary@ or @Data.ByteString.Base64@ modules.
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32
       ( Base32
       , encode
       , decode
--       , decodeLenient
       ) where

import Data.ByteString as BS
import Data.ByteString.Internal as BS
import Data.ByteString.Base32.Internal
import Data.List as L


-- | Base32 encoded bytestring.
type Base32 = ByteString

encW5 :: Word5 -> Word8
encW5 !x
  |  x <= 25  = 65 + x
  | otherwise = 24 + x
{-# INLINE encW5 #-}

encTable :: EncTable
encTable = BS.pack $ L.map encW5 [0..31]

-- | Encode a bytestring into base32 form.
encode :: ByteString -> Base32
encode = unpack5 encTable

decW5 :: Word8 -> Word5
decW5 !x
  | x <  50  {- c2w '2' -} = invIx
  | x <= 55  {- c2w '7' -} = x - 24 {- c2w '2' - 26 -}
  | x <  65  {- c2w 'A' -} = invIx
  | x <= 90  {- c2w 'Z' -} = x - 65 {- c2w 'A' -}
  | x <  97  {- c2w 'a' -} = invIx
  | x <= 122 {- c2w 'z' -} = x - 97 {- c2w 'a' -}
  | otherwise = invIx
{-# INLINE decW5 #-}

decTable :: ByteString
decTable = BS.pack $ L.map decW5 [minBound .. maxBound]

-- | Decode a base32 encoded bytestring.
decode :: Base32 -> ByteString
decode = pack5 decTable

decCharLenient :: Char -> Word5
decCharLenient x
  | x <  '2'  = err
  | x <= '7'  = 26 + fromIntegral (fromEnum x) - fromIntegral (fromEnum '2')
  | x <  'A'  = err
  | x <= 'Z'  = fromIntegral (fromEnum x) - fromIntegral (fromEnum 'A')
  | x <  'a'  = err
  | x <= 'z'  = fromIntegral (fromEnum x) - fromIntegral (fromEnum 'a')
  | otherwise = err
  where
    err = error "base32: decodeChar: out of range"

decW5Lenient :: Word8 -> Word5
decW5Lenient = decCharLenient . w2c
{-# INLINE decW5Lenient #-}

-- TODO padding leniency
-- | Case-insensitive counterpart of the 'decode'.
decodeLenient :: Base32 -> ByteString
decodeLenient = id -- pack5 nullPtr decW5Lenient