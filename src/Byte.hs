
module Byte(
  Byte(..),
  toUnsigned,
  ofUnsigned,
  addWithCarry,
  addForAuxCarry,
  ) where

import Data.Bits (Bits, (.&.))
import Data.Word8 (Word8)
import Text.Printf (printf)

newtype Byte = Byte { unByte :: Word8 }
  deriving (Eq,Ord,Num,Bits)

instance Show Byte where show = printf "%02X" . unByte

toUnsigned :: Byte -> Int
toUnsigned = fromIntegral . unByte

ofUnsigned :: Int -> Byte
ofUnsigned = Byte . fromIntegral

addWithCarry :: Bool -> Byte -> Byte -> (Byte,Bool)
addWithCarry cin x y = (ofUnsigned res, cout) where
    res :: Int = toUnsigned x + toUnsigned y + (if cin then 1 else 0)
    cout = res >= 256

addForAuxCarry :: Bool -> Byte -> Byte -> Bool
addForAuxCarry cin x y = aux where
    res = (x .&. 0xF) + (y .&. 0xF) + (if cin then 1 else 0)
    aux = res >= 16
