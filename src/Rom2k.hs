
module Rom2k (Rom,size,load,read) where

import Prelude hiding (read,init)

import Data.Array (Array,(!),listArray)
import Byte (Byte(..))
import qualified Data.ByteString as BS (readFile,unpack)

type Addr = Int

data Rom = Rom { bytesA :: Array Addr Byte }

size :: Int
size = 2048

init :: [Byte] -> Rom
init bytes = if
    | n == size -> Rom { bytesA = listArray (0,size-1) bytes }
    | otherwise -> error $ "Rom2k.init: #bytes=" <> show n <> " (expected:" <> show size <> ")"
    where
        n = length bytes

load :: FilePath -> IO Rom
load path = do
  byteString <- BS.readFile path
  let bs = map Byte $ BS.unpack byteString
  return $ init bs

read :: Rom -> Addr -> Byte
read rom a = if
  | inRange a -> bytesA rom ! a
  | otherwise -> error $ "Rom2k.read: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
