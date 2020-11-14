
module Mem (Mem,Addr,init,read,write) where

import Prelude hiding (init,read)

import Addr (Addr(..))
import Byte (Byte(..))
import Ram8k (Ram)
import Rom (Rom)
import qualified Ram8k (init,read,write)
import qualified Rom (lookup)

data Mem = Mem { rom :: Rom, ram :: Ram }

init :: Rom -> Mem
init rom = Mem { rom, ram = Ram8k.init }

read :: Mem -> Addr -> Byte
read Mem{rom,ram} a = do
  case Rom.lookup rom a of
    Just b -> b
    Nothing -> Ram8k.read ram a

write :: Mem -> Addr -> Byte -> Mem
write mem@Mem{ram} a b = mem { ram = Ram8k.write ram a b }
