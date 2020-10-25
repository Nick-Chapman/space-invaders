
module Ram8k (Ram,init,read,write) where -- imperative

import Prelude hiding (init,read)

import Data.Array.IO (IOArray,newArray,readArray,writeArray)

import Byte (Byte(..))

type Addr = Int

data Ram = Ram { arr :: IOArray Int Byte }

size :: Int
size = 8 * 1024

init :: IO Ram
init = do
  arr <- newArray (0,size-1) 0
  return $ Ram {arr}

read :: Ram -> Addr -> IO Byte
read Ram{arr} a = if
  | inRange a -> readArray arr a
  | otherwise -> error $ "Ram8k.read: " <> show a

write :: Ram -> Addr -> Byte -> IO ()
write Ram{arr} a b = if
  | inRange a -> writeArray arr a b
  | otherwise -> error $ "Ram8k.write: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
