
module Addr(
  Addr (..),
  toUnsigned, bump, fromHiLo, toHiLo,
  addCarryOut,
  ) where

import Byte(Byte(..))
import Data.Word (Word16)
import GHC.Arr (Ix)
import HiLo (HiLo(..))
import Text.Printf (printf)
import qualified Byte (ofUnsigned)

newtype Addr = Addr { unAddr :: Word16 }
  deriving (Eq,Ord,Num,Enum,Ix,Integral,Real)

instance Show Addr where show = printf "%04X" . unAddr

toUnsigned :: Addr -> Int
toUnsigned = fromIntegral . unAddr

bump :: Addr -> Int -> Addr
bump a n = Addr (unAddr a + fromIntegral n)

fromHiLo :: HiLo Byte -> Addr
fromHiLo HiLo{hi,lo} =
    Addr (256 * fromIntegral (unByte hi) + fromIntegral (unByte lo))

toHiLo :: Addr -> HiLo Byte
toHiLo a = HiLo{hi,lo} where
    lo = Byte.ofUnsigned (n `mod` 256)
    hi = Byte.ofUnsigned ( n `div` 256)
    n = fromIntegral $ unAddr a

addCarryOut :: Addr -> Addr -> (Addr, Bool)
addCarryOut a1 a2 = do
  let i :: Int = Addr.toUnsigned a1 + Addr.toUnsigned a2
  let cout = (i > 0xFFFF)
  (Addr (fromIntegral i), cout)
