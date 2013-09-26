-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Efficient encoding and decoding of base32hex encoded bytestring
--   according to RFC 4648. <http://tools.ietf.org/html/rfc4648>
--
--   This module recommended to be imported as @import
--   Data.ByteString.Base32.Hex as Base32Hex@ to avoid name clashes
--   with @Data.ByteString.Base32@.
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32.Hex
       ( encode
       , decode
       , decodeLenient
       ) where

import Data.ByteString as BS
import Data.ByteString.Base32.Internal
import Data.List as L


encW5 :: Word5 -> Word8
encW5 !x
  |  x <= 9   = 48 {- c2w '0' -}      + x
  | otherwise = 55 {- c2w 'A' - 10 -} + x

encTable :: EncTable
encTable = BS.pack $ L.map encW5 [0..31]

-- | Encode a bytestring into base32hex form.
encode :: ByteString -> ByteString
encode = unpack5 encTable

decW5 :: Word8 -> Word5
decW5 !x
  | x <  48  {- c2w '0' -} = invIx
  | x <= 57  {- c2w '9' -} = x - 48 {- c2w '0' -}
  | x <  65  {- c2w 'A' -} = invIx
  | x <= 86  {- c2w 'V' -} = x - 55 {- c2w 'A' + 10 -}
  | x <  97  {- c2w 'a' -} = invIx
  | x <= 118 {- c2w 'v' -} = x - 87 {- c2w 'a' + 10 -}
  | otherwise = invIx

decTable :: DecTable
decTable = BS.pack $ L.map decW5 [minBound .. maxBound]

-- | Decode a base32hex encoded bytestring.
decode :: ByteString -> ByteString
decode = pack5 decTable

decodeLenient :: ByteString -> ByteString
decodeLenient = undefined