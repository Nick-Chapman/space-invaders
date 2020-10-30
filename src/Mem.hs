
module Mem (Mem,Addr,init,initTst,read,write) where

import Prelude hiding (init,read)

import Addr (Addr(..))
import Byte (Byte(..))
import Ram8k (Ram)
import Rom2k (Rom,size)
import qualified Addr (toUnsigned)
import qualified Rom2k (read)
import qualified Ram8k (init,read,write)

import Data.Map (Map)
import qualified Data.Map.Strict as Map


data Mem = Mem {
  m_read :: (forall a. String -> a) -> Addr -> Byte,
  m_write :: (forall a. String -> a) -> Addr -> Byte -> Mem
  }

read :: (forall a. String -> a) -> Mem -> Addr -> Byte
read e Mem{m_read} a = m_read e a

write :: (forall a. String -> a) -> Mem -> Addr -> Byte -> Mem
write e Mem{m_write} a b = m_write e a b

type ERR = forall a. String -> a


init :: (Rom,Rom,Rom,Rom) -> Mem
init t = lift1 (init1 t)

lift1 :: Mem1 -> Mem
lift1 m1 = do
  let m_read (e::ERR) a = read1 e m1 a
  let m_write (e::ERR) a b = lift1 (write1 e m1 a b)
  Mem { m_read, m_write }

data Mem1 = Mem1
  { e :: Rom
  , f :: Rom
  , g :: Rom
  , h :: Rom
  , ram :: Ram
  }

init1 :: (Rom,Rom,Rom,Rom) -> Mem1
init1 (e,f,g,h) = Mem1 {e,f,g,h, ram = Ram8k.init}

read1 :: (forall a. String -> a) -> Mem1 -> Addr -> Byte
read1 error Mem1{e,f,g,h,ram} a = if
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

write1 :: (forall a. String -> a) -> Mem1 -> Addr -> Byte -> Mem1
write1 error mem@Mem1{ram} a b = if
  | i < k8 -> mem --error $ "Mem.write: " <> show a <> " -- cant write to rom"
  | i < k16 -> mem { ram = Ram8k.write ram (i - k8) b }
  | i < k24 -> mem { ram = Ram8k.write ram (i - k16) b } -- one mirror needed?
  | otherwise -> error $ "Mem.write: " <> show a
  where
    i = Addr.toUnsigned a
    k8 = Rom2k.size * 4
    k16 = Rom2k.size * 8
    k24 = Rom2k.size * 12



initTst :: [Byte]-> Mem
initTst bs = lift2 (init2 bs)


lift2 :: Mem2 -> Mem
lift2 m2 = do
  let m_read (e::ERR) a = read2 e m2 a
  let m_write (e::ERR) a b = lift2 (write2 e m2 a b)
  Mem { m_read, m_write }


data Mem2 = Mem2 { m :: Map Addr Byte }

init2 :: [Byte]-> Mem2
init2 bs = do
  let m = Map.fromList [ (Addr i, b) | (i,b) <- zip [0..] bs ]
  Mem2 { m }

read2 :: (forall a. String -> a) -> Mem2 -> Addr -> Byte
read2 _ Mem2{m} a = Map.findWithDefault (Byte 0) a m

write2 :: (forall a. String -> a) -> Mem2 -> Addr -> Byte -> Mem2
write2 _ Mem2{m} a b = Mem2 { m = Map.insert a b m }
