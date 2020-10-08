
module Emulate (emulate) where

import Addr (Addr)
import Byte (Byte(..))
import Cpu (Cpu)
import Effect (Eff(..),getPC,setPC)
import Execute (execute)
import HiLo (HiLo(..))
import InstructionSet (decode)
import Mem (Mem)
import Phase (Phase)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Cpu (init,get,set)
import qualified Mem (read)
import qualified Phase (Byte,Addr)

data EmuTime

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr

startAddr :: Addr
startAddr = Addr.fromHiLo $ HiLo { hi = Byte 0, lo = Byte 0 }

theSemantics :: Eff EmuTime ()
theSemantics = do
  setPC startAddr
  loop
    where
      loop = do
        a0 <- getPC
        byte <- ReadMem a0
        let op = decode a0 byte
        Trace (a0,byte,op)
        a1 <- execute a0 op
        setPC a1
        loop

emulate :: Mem -> IO ()
emulate mem0 = run (state0 mem0) theSemantics $ \_ -> return
  where
    run :: State -> Eff EmuTime a -> (State -> a -> IO ()) -> IO ()
    run s@State{cpu,mem} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k
      GetReg r -> k s (Cpu.get cpu r)
      SetReg r b -> k s { cpu = Cpu.set cpu r b} ()
      ReadMem a -> k s (Mem.read mem a)
      SplitAddr a -> k s (Addr.toHiLo a)
      MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      IncAddr a -> k s (Addr.bump a 1)
      Trace x -> do
        print (x,cpu)
        k s ()

data State = State { cpu :: Cpu Byte, mem :: Mem }

state0 :: Mem -> State
state0 mem = State { cpu = Cpu.init (Byte 0), mem }
