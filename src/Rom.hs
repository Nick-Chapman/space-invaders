
module Rom (Rom,fromBytes,loadInvaders,size,lookup) where

import Prelude hiding (lookup)

import Addr (Addr)
import Byte (Byte(..))
import Control.Monad (unless)
import Data.Array (Array,(!),listArray)
import Data.Word8 (Word8)
import qualified Data.ByteString as BS (readFile,unpack)

data Rom = Rom { size :: Addr, bytesA :: Array Addr Byte }

fromBytes :: [Byte] -> Rom
fromBytes bytes = do
  let size = fromIntegral (length bytes)
  let bytesA = listArray (0,size-1) bytes
  Rom { size, bytesA }

loadInvaders :: IO Rom
loadInvaders = do
  e <- loadBytes "roms/invaders.e"
  f <- loadBytes "roms/invaders.f"
  g <- loadBytes "roms/invaders.g"
  h <- loadBytes "roms/invaders.h"
  let bytes = map Byte (h ++ g ++ f ++ e)
  let size = length bytes
  let expected = 8192 -- 8k
  unless (size == expected) $
    error $ "Rom2k.init: #bytes=" <> show size <> " (expected:" <> show expected <> ")"
  return $ fromBytes bytes

loadBytes :: FilePath -> IO [Word8]
loadBytes path = BS.unpack <$> BS.readFile path

lookup :: Rom -> Addr -> Maybe Byte
lookup Rom{size,bytesA} a = if
  | a < size -> Just (bytesA ! a)
  | otherwise -> Nothing
