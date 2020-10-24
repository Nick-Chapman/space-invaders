
module Byte(
  Byte(..),
  toUnsigned,
  ofUnsigned,
  addWithCarry,
  addForAuxCarry,
  decimalAdjust
  ) where

import Data.Bits (Bits,(.&.),(.|.))
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


decimalAdjust :: Bool -> Bool -> Byte -> (Byte,Bool,Bool)
decimalAdjust auxIn cin byteIn = do

  let lo = byteIn .&. 0xF
  let loNeedsAdjust = lo > 9 || auxIn
  let loAdjust = if loNeedsAdjust then lo + 6 else lo
  let auxOut = loAdjust >= 16

  let hi = (byteIn .&. 0xF0) + (if auxOut then 16 else 0)
  let hiNeedsAdjust = hi > 0x90 || cin
  let hiAdjust = if hiNeedsAdjust then hi + 0x60 else hi
  let cout = hiAdjust >= 256

  let byteOut = hiAdjust .|. (loAdjust .&. 0xF)
  (byteOut,auxOut,cout)
