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
       ( encode
       , decode
       , decodeLenient
       ) where

import Data.Bits.Extras
import Data.ByteString as BS
import Data.ByteString.Internal as BS
import Data.Word
import Foreign   hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Endian


{-----------------------------------------------------------------------
-- Utils
-----------------------------------------------------------------------}

type Word5 = Word8

-- System.Endian.toBE32 is slower because toBE32 implemented using
-- cbits shuffle functions while toBE32' implemented used gcc
-- intrinsics
--
toBE64' :: Word64 -> Word64
toBE64' = if getSystemEndianness == BigEndian then id else byteSwap
{-# INLINE toBE64' #-}

toBE32' :: Word32 -> Word32
toBE32' = if getSystemEndianness == BigEndian then id else byteSwap
{-# INLINE toBE32' #-}

fromBE32' :: Word32 -> Word32
fromBE32' = toBE32'
{-# INLINE fromBE32' #-}

-- n = 2 ^ d
padCeilN :: Int -> Int -> Int
padCeilN !n !x
  | remd == 0 = x
  | otherwise = (x - remd) + n
  where  mask = n - 1
         remd = x .&. mask

{-----------------------------------------------------------------------
-- Encoding
-----------------------------------------------------------------------}

type EncTable = Ptr Word8

unpack5 :: EncTable -> ByteString -> ByteString
unpack5 !tbl bs @ (PS fptr off sz) =
  unsafePerformIO $ do
    let unpackedSize = dstSize $ BS.length bs
    BS.create unpackedSize $ \ dst -> do
        withForeignPtr fptr $ \ ptr -> do
          dst_end <- bigStep dst (advancePtr ptr off) sz
          _ <- fillPadding dst_end (unpackedSize - (dst_end `minusPtr` dst))
          return ()
  where
    dstSize x = padCeilN 8 (d + if m == 0 then 0 else 1)
      where (d, m) = (x * 8) `quotRem` 5

    fillPadding dst s = memset dst (c2w '=') (fromIntegral s)

    bigStep !dst !src !s
      |     s >= 5  = do
        unpack5_40 dst src
        bigStep (dst `advancePtr` 8) (src `advancePtr` 5) (s - 5)
      |   otherwise  = smallStep dst src s 0 0

    unpack5_40 !dst !src = do
      w32he <- peek (castPtr src) :: IO Word32
      let w32 = toBE32' w32he
      fill8_32 0 (w32 `unsafeShiftR` 27)
      fill8_32 1 (w32 `unsafeShiftR` 22)
      fill8_32 2 (w32 `unsafeShiftR` 17)
      fill8_32 3 (w32 `unsafeShiftR` 12)
      fill8_32 4 (w32 `unsafeShiftR` 7)
      fill8_32 5 (w32 `unsafeShiftR` 2)

      w8 <- peekElemOff src 4
      fill8_32 6 (             (w32 `unsafeShiftL` 3)
              .|. fromIntegral (w8  `unsafeShiftR` 5))
      fill8_32 7 (fromIntegral w8)
     where
      fill8_32 :: Int -> Word32 -> IO ()
      fill8_32 !i !w32 = do
        w8 <- peekByteOff tbl (fromIntegral w32 .&. 0x1f)
        poke (dst `advancePtr` i) w8

    smallStep !dst !src !s !unused !un_cnt
      | un_cnt >= 5 = do
        let ix = unused `unsafeShiftR` 3
        peekByteOff tbl (fromIntegral ix) >>= poke dst
        smallStep (advancePtr dst 1)
                  src s
                 (unused `unsafeShiftL` 5)
                 (un_cnt - 5)

      |    s == 0   = do
        if un_cnt == 0
          then return dst
          else do
            let ix = unused `unsafeShiftR` 3
            peekByteOff tbl (fromIntegral ix) >>= poke dst
            return (dst `advancePtr` 1)

      |  otherwise  = do
        w8 <- peek src
        let usd_cnt = 5 - un_cnt
        let bits    = w8 .&. complement (bit (8 - usd_cnt) - 1)
        let ix = (unused .|. bits `shiftR` un_cnt) `unsafeShiftR` 3
        peekByteOff tbl (fromIntegral ix) >>= poke dst
        smallStep (advancePtr dst 1)
                  (advancePtr src 1) (pred s)
                  (w8 `shiftL` usd_cnt) (8 - usd_cnt)

encW5 :: Word5 -> Word8
encW5 !x
  |  x <= 25  = 65 + x
  | otherwise = 24 + x
{-# INLINE encW5 #-}

encTable :: ForeignPtr Word8
PS encTable _ _ = BS.pack $ fmap encW5 [0..31]

-- | Encode a bytestring into base32 form.
encode :: ByteString -> ByteString
encode bs =
  unsafePerformIO $ do
    withForeignPtr encTable $ \ptr -> do
      return $ unpack5 ptr bs

{-----------------------------------------------------------------------
-- Decoding
-----------------------------------------------------------------------}

type DecTable = Ptr Word5

invIx :: Word5
invIx = 255

pack5 :: DecTable -> ByteString -> ByteString
pack5 !tbl bs @ (PS fptr off sz) =
  unsafePerformIO $ do
    let packedSize = dstSize $ BS.length bs
    BS.createAndTrim packedSize $ \ dst -> do
        withForeignPtr fptr $ \ ptr -> do
          dst_end <- bigStep dst (advancePtr ptr off) sz
          return (dst_end `minusPtr` dst)
  where
    lookupTable :: Word8 -> Word5
    lookupTable ix
        | x == invIx = error $ "base32: decode: invalid character" ++ show ix
        | otherwise  = x
      where x = inlinePerformIO (peekByteOff tbl (fromIntegral ix))
    {-# INLINE lookupTable #-}

    dstSize x = d + if m == 0 then 0 else 1
      where (d, m) = (x * 5) `quotRem` 8

    bigStep !dst !src !s
      | s > 8 = do
        pack5_40 dst src
        bigStep (dst `advancePtr` 5) (src `advancePtr` 8) (s - 8)
      | otherwise = smallStep dst src s (0 :: Word64) 0

    pack5_40 !dst !src = do
        w64he <- peek (castPtr src) :: IO Word64
        let w64 = toBE64' w64he
        let w40 = putAsW5 (w64 `unsafeShiftR` 00) $
                  putAsW5 (w64 `unsafeShiftR` 08) $
                  putAsW5 (w64 `unsafeShiftR` 16) $
                  putAsW5 (w64 `unsafeShiftR` 24) $
                  putAsW5 (w64 `unsafeShiftR` 32) $
                  putAsW5 (w64 `unsafeShiftR` 40) $
                  putAsW5 (w64 `unsafeShiftR` 48) $
                  putAsW5 (w64 `unsafeShiftR` 56) 0
        pokeW40 w40
      where
        putAsW5 :: Word64 -> Word64 -> Word64
        {-# INLINE putAsW5 #-}
        putAsW5 !w8 !acc = (acc `unsafeShiftL` 5)
                       .|. fromIntegral (lookupTable (fromIntegral w8))

        pokeW40 :: Word64 -> IO ()
        {-# INLINE pokeW40 #-}
        pokeW40 !w40 = do
          poke dst (fromIntegral (w40 `unsafeShiftR` 32) :: Word8)
          poke (castPtr (dst `advancePtr` 1))
               (fromBE32' (fromIntegral w40 :: Word32))

    smallStep !dst !src !s !unused !un_cnt
      | un_cnt >= 8 = do
        poke dst $ fromIntegral (unused `unsafeShiftR` (un_cnt - 8))
        smallStep (dst `advancePtr` 1) src s unused (un_cnt - 8)

      |   s == 0  = return dst
      | otherwise = do
        w8 <- peek src
        if w2c w8 == '='
           then if (bit un_cnt - 1) .&. unused == 0
                then smallStep dst src 0 0 0
                else smallStep dst src 0 (unused `shiftL` (8 - un_cnt)) 8
           else smallStep dst
                  (src `advancePtr` 1) (pred s)
                  ((unused `unsafeShiftL` 5)
                   .|. fromIntegral (lookupTable (fromIntegral w8)))
                  (un_cnt + 5)

decW5 :: Word8 -> Word5
decW5 !x
  | x <  50 {- c2w '2' -} = invIx
  | x <= 55 {- c2w '7' -} = x - 24
  | x <  65 {- c2w 'A' -} = invIx
  | x <= 90 {- c2w 'Z' -} = x - 65
  |       otherwise       = invIx
{-# INLINE decW5 #-}

decTable :: ForeignPtr Word8
PS decTable _ _ = BS.pack $ fmap decW5 [minBound .. maxBound]

-- | Decode a base32 encoded bytestring.
decode :: ByteString -> ByteString
decode bs =
  unsafePerformIO $ do
    withForeignPtr decTable $ \tbl ->
      return $ pack5 tbl bs

{-----------------------------------------------------------------------
-- Lenient Decoding
-----------------------------------------------------------------------}

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
decodeLenient :: ByteString -> ByteString
decodeLenient = id -- pack5 nullPtr decW5Lenient