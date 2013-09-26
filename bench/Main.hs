module Main (main) where

import Criterion.Main
import Data.ByteString as BS
import Data.ByteString.Base32 as Base32


main :: IO ()
main = defaultMain
  [ bench "encode/1M" $ nf encode $ BS.replicate 1000000 0x8e
  , bench "encode/5M" $ nf encode $ BS.replicate 5000000 0x8e

  , bench "decode/regular/1M" $ nf decode $ BS.replicate 1000000 0x41
  , bench "decode/regular/5M" $ nf decode $ BS.replicate 5000000 0x41

  , bench "decode/lenient/1M" $ nf decodeLenient $ BS.replicate 1000000 0x41
  , bench "decode/lenient/5M" $ nf decodeLenient $ BS.replicate 5000000 0x41
  ]