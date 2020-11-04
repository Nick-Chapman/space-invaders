
module HiLo (HiLo(..)) where

data HiLo a = HiLo { lo :: a, hi :: a }
  deriving (Eq,Show)
