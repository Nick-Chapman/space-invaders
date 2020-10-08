
module Byte(
  Byte(..),
  toUnsigned,
  ofUnsigned
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
