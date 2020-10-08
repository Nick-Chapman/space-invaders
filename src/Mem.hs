
module Mem (Mem,Addr,init,read,write) where

import Prelude hiding (init,read)

import Addr (Addr)
import Byte (Byte(..))
import Rom2k (Rom,size)
import qualified Addr (toUnsigned)
import qualified Rom2k (read)

data Mem = Mem
  { e :: Rom
  , f :: Rom
  , g :: Rom
  , h :: Rom
  }

init :: (Rom,Rom,Rom,Rom) -> Mem
init (e,f,g,h) = Mem {e,f,g,h}

read :: Mem -> Addr -> Byte
read Mem{e,f,g,h} a = if
  | i < k2 -> Rom2k.read h i
  | i < k4 -> Rom2k.read g (i - k2)
  | i < k6 -> Rom2k.read f (i - k4)
  | i < k8 -> Rom2k.read e (i - k6)
  | otherwise -> error $ "Mem.read: " <> show a
  where
    i = Addr.toUnsigned a
    k2 = Rom2k.size
    k4 = Rom2k.size * 2
    k6 = Rom2k.size * 3
    k8 = Rom2k.size * 4

write :: Mem -> Addr -> Byte -> Mem
write = undefined
