
module Byte(
  Byte(..),
  toUnsigned,
  ofUnsigned,
  adc, -- add with carry
  ) where

import Data.Bits (Bits)
import Data.Word8 (Word8)
import Text.Printf (printf)

newtype Byte = Byte { unByte :: Word8 }
  deriving (Eq,Ord,Num,Bits)

instance Show Byte where show = printf "%02X" . unByte

toUnsigned :: Byte -> Int
toUnsigned = fromIntegral . unByte

ofUnsigned :: Int -> Byte
ofUnsigned = Byte . fromIntegral

adc :: Bool -> Byte -> Byte -> (Byte,Bool)
adc cin x y = (Byte $ fromIntegral res,cout) where
    res = toUnsigned x + toUnsigned y + (if cin then 1 else 0)
    cout = res >= 256
