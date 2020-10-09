
module Byte(
  Byte(..),
  toUnsigned,
  ofUnsigned,
  decrement,
  ) where

import Data.Word8 (Word8)
import Text.Printf (printf)

newtype Byte = Byte { unByte :: Word8 }
  deriving (Eq,Ord,Num)

instance Show Byte where show = printf "%02X" . unByte

toUnsigned :: Byte -> Int
toUnsigned = fromIntegral . unByte

ofUnsigned :: Int -> Byte
ofUnsigned = Byte . fromIntegral

decrement :: Byte -> Byte
decrement b = b - 1 -- this does modulus 256.
