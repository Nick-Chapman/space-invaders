
module Ram8k (Ram,init,read,write) where

import Prelude hiding (init,read)

import Byte (Byte(..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Addr = Int

data Ram = Ram { m :: Map Int Byte }

size :: Int
size = 8 * 1024

init :: Ram
init = Ram { m = Map.empty }

read :: Ram -> Addr -> Byte
read Ram{m} a = if
  | inRange a -> Map.findWithDefault (Byte 0) a m
  | otherwise -> error $ "Ram8k.read: " <> show a

write :: Ram -> Addr -> Byte -> Ram
write Ram{m} a b = if
  | inRange a -> Ram { m = Map.insert a b m }
  | otherwise -> error $ "Ram8k.write: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
