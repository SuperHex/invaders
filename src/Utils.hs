module Utils where

import           Data.Bits
import           Data.Word (Word16, Word8)

encode :: (Integral a0, Integral a1, Num b, Bits b) => a0 -> a1 -> b
encode h l = fromIntegral h `shiftL` 8 .|. fromIntegral l

decode :: Word16 -> (Word8, Word8)
decode word = ( fromIntegral $ (word .&. 0xFF00) `shiftR` 8
              , fromIntegral $ word .&. 0x00FF )

mask :: Word8 -> [Word8]
mask word = map (\x -> word `shiftR` x .&. 0x01) [7,6..0]
