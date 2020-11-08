
module Shifter (Shifter(..),Reg(..),init,get,set,allRegs) where

import Prelude hiding (init)

import Phase (Byte)

data Shifter p = Shifter
  { hi :: Byte p
  , lo :: Byte p
  , off :: Byte p
  }

instance Show (Byte p) => Show (Shifter p) where
  show Shifter{hi,lo,off} =
    "shifter(" <> show off <> "," <> show hi <> show lo <> ")"

data Reg = HI | LO | OFF
  deriving (Eq)

instance Show Reg where
  show = \case
    HI -> "Shifter_HI"
    LO -> "Shifter_LO"
    OFF -> "Shifter_OFF"

allRegs :: [Reg]
allRegs = [HI,LO,OFF]

init :: Byte p -> Shifter p
init zero = Shifter
  { hi = zero
  , lo = zero
  , off = zero
  }

get :: Shifter p -> Reg -> Byte p
get Shifter{hi,lo,off} = \case
  HI -> hi
  LO -> lo
  OFF -> off

set :: Shifter p -> Reg -> Byte p -> Shifter p
set shifter reg x = case reg of
  HI -> shifter { hi = x }
  LO -> shifter { lo = x }
  OFF -> shifter { off = x }
