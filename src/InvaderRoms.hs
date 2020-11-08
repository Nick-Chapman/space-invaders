
module InvaderRoms (Roms,load,lookup) where

import Addr (Addr)
import Byte (Byte)
import Prelude hiding (lookup)
import Rom2k (Rom)
import qualified Addr (toUnsigned)
import qualified Rom2k (load,read,size)

data Roms = Roms
  { e :: Rom
  , f :: Rom
  , g :: Rom
  , h :: Rom
  }

load :: IO Roms
load = do
  e <- Rom2k.load "roms/invaders.e"
  f <- Rom2k.load "roms/invaders.f"
  g <- Rom2k.load "roms/invaders.g"
  h <- Rom2k.load "roms/invaders.h"
  return $ Roms {e,f,g,h}

lookup :: Roms -> Addr -> Maybe Byte
lookup Roms{e,f,g,h} a = do
  let i :: Int = Addr.toUnsigned a
  if
    | i < k2 -> Just (Rom2k.read h i)
    | i < k4 -> Just (Rom2k.read g (i - k2))
    | i < k6 -> Just (Rom2k.read f (i - k4))
    | i < k8 -> Just (Rom2k.read e (i - k6))
    | otherwise -> Nothing
    where
      k2 = Rom2k.size
      k4 = Rom2k.size * 2
      k6 = Rom2k.size * 3
      k8 = Rom2k.size * 4
