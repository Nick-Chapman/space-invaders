
module Mem (Mem,Addr,initInvader,initTst,read,write) where

import Prelude hiding (read)

import Addr (Addr(..))
import Byte (Byte(..))
import Data.Map (Map)
import InvaderRoms (Roms)
import Ram8k (Ram)
import qualified Addr (toUnsigned)
import qualified Data.Map.Strict as Map (fromList,findWithDefault,insert)
import qualified InvaderRoms (load,lookup)
import qualified Ram8k (init,read,write)

data Mem = Mem {
  m_read :: (forall a. String -> a) -> Addr -> Byte,
  m_write :: (forall a. String -> a) -> Addr -> Byte -> Mem
  }

read :: (forall a. String -> a) -> Mem -> Addr -> Byte
read e Mem{m_read} a = m_read e a

write :: (forall a. String -> a) -> Mem -> Addr -> Byte -> Mem
write e Mem{m_write} a b = m_write e a b

type ERR = forall a. String -> a

initInvader :: IO Mem
initInvader = do
  roms <- InvaderRoms.load
  return $ lift1 (init1 roms)

lift1 :: Mem1 -> Mem
lift1 m1 = do
  let m_read (e::ERR) a = read1 e m1 a
  let m_write (e::ERR) a b = lift1 (write1 e m1 a b)
  Mem { m_read, m_write }

data Mem1 = Mem1
  { roms :: Roms
  , ram :: Ram
  }

init1 :: Roms -> Mem1
init1 roms = Mem1 {roms, ram = Ram8k.init}

read1 :: (forall a. String -> a) -> Mem1 -> Addr -> Byte
read1 error Mem1{roms,ram} a = do
  case InvaderRoms.lookup roms a of
    Just b -> b
    Nothing -> if
      | i < k16 -> Ram8k.read ram (i - k8)
      | otherwise -> error $ "Mem.read: " <> show a
      where
        i = Addr.toUnsigned a
        k8 = 0x2000
        k16 = k8 * 2

write1 :: (forall a. String -> a) -> Mem1 -> Addr -> Byte -> Mem1
write1 error mem@Mem1{ram} a b = if
  | i < k8 -> mem --error $ "Mem.write: " <> show a <> " -- cant write to rom"
  | i < k16 -> mem { ram = Ram8k.write ram (i - k8) b }
  | i < k24 -> mem { ram = Ram8k.write ram (i - k16) b } -- one mirror needed?
  | otherwise -> error $ "Mem.write: " <> show a
  where
    i = Addr.toUnsigned a
    k8 = 0x2000
    k16 = k8 * 2
    k24 = k8 * 3


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
