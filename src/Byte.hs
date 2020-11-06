
module Byte(
  Byte(..),
  toUnsigned,
  ofUnsigned,
  addWithCarry,
  ) where

import Data.Bits
import Data.Word8 (Word8)
import Text.Printf (printf)

newtype Byte = Byte { unByte :: Word8 }
  deriving (Eq,Ord,Num,Bits,Enum)

instance Show Byte where show = printf "%02X" . unByte

toUnsigned :: Byte -> Int
toUnsigned = fromIntegral . unByte

ofUnsigned :: Int -> Byte
ofUnsigned = Byte . fromIntegral

addWithCarry :: Bool -> Byte -> Byte -> (Byte,Bool)
addWithCarry cin x y = (ofUnsigned res, cout) where
    res :: Int = toUnsigned x + toUnsigned y + (if cin then 1 else 0)
    cout = res >= 256
