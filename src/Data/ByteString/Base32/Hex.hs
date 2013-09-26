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
module Data.ByteString.Base32.Hex
       ( encode
       , decode
       , decodeLenient
       ) where

import Data.ByteString as BS


-- | Encode a bytestring into base32hex form.
encode :: ByteString -> ByteString
encode = undefined

-- | Decode a base32hex encoded bytestring.
decode :: ByteString -> ByteString
decode = undefined

decodeLenient :: ByteString -> ByteString
decodeLenient = undefined