
module Mem (Mem,Addr,init,read,write) where

import Prelude hiding (init,read)

import Addr (Addr)
import Byte (Byte(..))
import Ram8k (Ram)
import Rom2k (Rom,size)
import qualified Addr (toUnsigned)
import qualified Rom2k (read)
import qualified Ram8k (init,read,write)

data Mem = Mem
  { e :: Rom
  , f :: Rom
  , g :: Rom
  , h :: Rom
  , ram :: Ram
  }

init :: (Rom,Rom,Rom,Rom) -> Mem
init (e,f,g,h) = Mem {e,f,g,h, ram = Ram8k.init}

read :: (forall a. String -> a) -> Mem -> Addr -> Byte
read error Mem{e,f,g,h,ram} a = if
  | i < k2 -> Rom2k.read h i
  | i < k4 -> Rom2k.read g (i - k2)
  | i < k6 -> Rom2k.read f (i - k4)
  | i < k8 -> Rom2k.read e (i - k6)
  | i < k16 -> Ram8k.read ram (i - k8)
  | otherwise -> error $ "Mem.read: " <> show a
  where
    i = Addr.toUnsigned a
    k2 = Rom2k.size
    k4 = Rom2k.size * 2
    k6 = Rom2k.size * 3
    k8 = Rom2k.size * 4
    k16 = Rom2k.size * 8

write :: (forall a. String -> a) -> Mem -> Addr -> Byte -> Mem
write error mem@Mem{ram} a b = if
  | i < k8 -> error $ "Mem.write: " <> show a <> " -- cant write to rom"
  | i < k16 -> mem { ram = Ram8k.write ram (i - k8) b }
  -- | i < k24 -> mem { ram = Ram8k.write ram (i - k16) b } -- one mirror needed?
  | otherwise -> error $ "Mem.write: " <> show a
  where
    i = Addr.toUnsigned a
    k8 = Rom2k.size * 4
    k16 = Rom2k.size * 8
    --k24 = Rom2k.size * 12
