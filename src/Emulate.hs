
module Emulate (emulate) where

import Addr (Addr)
import Byte (Byte(..))
import Cpu (Cpu)
import Effect (Eff(..),getPC,setPC)
import Execute (Flow(..),execute0,execute1,execute2)
import HiLo (HiLo(..))
import InstructionSet (Op(..),Op0,Op1,Op2,decode)
import Mem (Mem)
import Phase (Phase)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Cpu (init,get,set)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr)

data EmuTime

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr

startAddr :: Addr
startAddr = Addr.fromHiLo $ HiLo { hi = Byte 0, lo = Byte 0 }

data Instruction -- op+args
  = Ins0 Op0
  | Ins1 Op1 Byte
  | Ins2 Op2 Byte Byte

instance Show Instruction where
  show = \case
    Ins0 op0 -> show op0
    Ins1 op1 b1 -> unwords [show op1, show b1]
    Ins2 op2 b1 b2 -> unwords [show op2, show b1, show b2]

fetch :: Eff EmuTime Byte -- fetch byte at PC, and increment PC
fetch = do
  pc <- getPC
  byte <- ReadMem pc
  OffsetAddr 1 pc >>= setPC
  return byte

fetchImmediates :: Op -> Eff EmuTime Instruction
fetchImmediates = \case
  Op0 op0 -> return (Ins0 op0)
  Op1 op1 -> do
    b1 <- fetch
    return (Ins1 op1 b1)
  Op2 op2 -> do
    b1 <- fetch
    b2 <- fetch
    return (Ins2 op2 b1 b2)

execute :: Instruction -> Eff EmuTime (Flow EmuTime)
execute = \case
  Ins0 op0 -> execute0 op0
  Ins1 op1 b1 -> execute1 op1 b1
  Ins2 op2 b1 b2 -> execute2 op2 (b1,b2)

theSemantics :: Eff EmuTime ()
theSemantics = do
  setPC startAddr
  loop
    where
      loop = do
        pc <- getPC
        byte <- fetch
        let op = decode pc byte -- Pass pc to decode for improved error
        instruction <- fetchImmediates op
        --Trace (Step pc instruction)
        execute instruction >>= \case
          Next -> return ()
          Jump a -> setPC a
        Trace (Step pc instruction)
        loop

data Step = Step Addr Instruction

instance Show Step where
  show (Step pc instruction) = show pc <> " : " <> show instruction

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
      WriteMem a b -> k s { mem = Mem.write mem a b } ()
      SplitAddr a -> k s (Addr.toHiLo a)
      MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      OffsetAddr n a -> k s (Addr.bump a n)
      Trace x -> do
        putStrLn (ljust 25 (show x) ++ show cpu)
        k s ()

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

data State = State { cpu :: Cpu Byte, mem :: Mem }

state0 :: Mem -> State
state0 mem = State { cpu = Cpu.init (Byte 0), mem }
