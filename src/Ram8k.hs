
module Ram8k (Ram,init,read,write) where

import Prelude hiding (init,read)

import Addr (Addr)
import Byte (Byte(..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Ram = Ram { m :: Map Addr Byte }

size :: Addr
size = 8192 -- 8k

init :: Ram
init = Ram { m = Map.empty }

read :: Ram -> Addr -> Byte
read Ram{m} a = Map.findWithDefault (Byte 0) (a `mod` size) m

write :: Ram -> Addr -> Byte -> Ram
write Ram{m} a b = Ram { m = Map.insert (a `mod` size) b m }
