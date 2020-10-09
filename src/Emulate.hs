
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
import Text.Printf (printf)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Byte (decrement,isZero)
import qualified Cpu (init,get,set,getFlagZ,setFlagZ)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr,Ticks)


-- | Ticks of the 2 MHz clock
newtype Ticks = Ticks { unTicks :: Int } deriving (Num)

instance Show Ticks where show = printf "[%05d]" . unTicks


data EmuTime -- At Emulation type we have concrete Bytes

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr
  type Ticks EmuTime = Ticks

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
    run s@State{ticks,cpu,mem} eff k = case eff of
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

      Decode (pc,byte) -> do
        case decode byte of
          Just op -> k s op
          Nothing -> do
            putStrLn (show ticks <> " " <> show pc <> " : " <> show byte)
            error $ "Decode: " <> show byte

      Decrement b -> k s (Byte.decrement b)

      SetFlagZ b -> k s { cpu = Cpu.setFlagZ cpu b } ()
      TestFlagZ -> do
        let z = Cpu.getFlagZ cpu
        let pred = Byte.isZero z
        --putStrLn $ "- TestFlagZ (" <> show z <> ") -> " <> show pred
        k s pred

      Advance n -> k s { ticks = ticks + fromIntegral n } ()
      Now{} -> k s ticks

      InstructionCycle eff -> do
        let pcBefore = programCounter s
        --putStrLn (ljust 50 (show ticks) ++ show cpu)
        run s eff $ \s@State{cpu} instruction -> do
          putStrLn (ljust 50 (prettyStep ticks pcBefore instruction) ++ show cpu)
          --putStrLn (prettyStep ticks pc instruction)
          k s ()

prettyStep :: Ticks -> Addr -> Instruction Byte -> String
prettyStep ticks pc instruction =
  show ticks <> " " <> show pc <> " : " <> show instruction

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

data State = State { ticks :: Ticks, cpu :: Cpu Byte, mem :: Mem }

state0 :: Mem -> State
state0 mem = State { ticks = 0, cpu = Cpu.init (Byte 0), mem }

programCounter :: State -> Addr
programCounter State{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}
