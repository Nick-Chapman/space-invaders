
module Emulate (emulate) where

import Addr (Addr)
import Byte (Byte(..))
import Cpu (Cpu,Reg(..))
import Effect (Eff(..))
import Semantics (setPC,fetchDecodeExec)
import HiLo (HiLo(..))
import InstructionSet (Instruction,decode)
import Mem (Mem)
import Phase (Phase)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Byte (decrement,isZero)
import qualified Cpu (init,get,set,getFlagZ,setFlagZ)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr)

data EmuTime -- At Emulation type we have concrete Bytes

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
        fetchDecodeExec
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
      ReadMem a -> do
        let b = Mem.read mem a
        --putStrLn $ "- ReadMem (" <> show a <> ") --> " <> show b
        k s b
      WriteMem a b -> do
        --putStrLn $ "- WriteMem (" <> show a <> ") = " <> show b
        k s { mem = Mem.write mem a b } ()
      SplitAddr a -> k s (Addr.toHiLo a)
      MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      OffsetAddr n a -> k s (Addr.bump a n)
      Decode (pc,b) -> k s (decode pc b)
      Decrement b -> k s (Byte.decrement b)

      SetFlagZ b -> k s { cpu = Cpu.setFlagZ cpu b } ()
      TestFlagZ -> do
        let z = Cpu.getFlagZ cpu
        let pred = Byte.isZero z
        --putStrLn $ "- TestFlagZ (" <> show z <> ") -> " <> show pred
        k s pred

      InstructionCycle eff -> do
        let pc = programCounter s
        putStrLn (ljust 45 "" ++ show cpu)
        run s eff $ \s instruction -> do
          --putStrLn (ljust 45 (prettyStep pc instruction) ++ show cpu)
          putStrLn (prettyStep pc instruction)
          k s ()

prettyStep :: Addr -> Instruction Byte -> String
prettyStep pc instruction =
  show pc <> " : " <> show instruction

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

data State = State { cpu :: Cpu Byte, mem :: Mem }

state0 :: Mem -> State
state0 mem = State { cpu = Cpu.init (Byte 0), mem }

programCounter :: State -> Addr
programCounter State{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}
