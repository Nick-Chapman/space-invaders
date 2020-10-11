
module Emulate (emulate) where

import Control.Monad (when)

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
import qualified Addr (fromHiLo,toHiLo,bump,add)
import qualified Cpu (init,get,set,getFlagZ,setFlagZ)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr,Ticks)


-- | Ticks of the 2 MHz clock
newtype Ticks = Ticks { unTicks :: Int } deriving (Num)

instance Show Ticks where show = printf "[%6d]" . unTicks


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

      Decode (pc,byte) -> do
        case decode byte of
          Just op -> k s op
          Nothing -> do
            putStrLn (show (ticks s) <> " " <> show pc <> " : " <> show byte)
            error $ "Decode: " <> show byte

      -- Byte ops
      Decrement b -> k s (b - 1)
      Subtract b1 b2 -> k s (b1 - b2)

      -- Word (Address) ops
      Add16 a1 a2 -> k s (Addr.add a1 a2) -- TODO: dont loose carry

      SetFlagZ b -> k s { cpu = Cpu.setFlagZ cpu b } ()
      TestFlagZ -> do
        let z = Cpu.getFlagZ cpu
        let pred = (z == 0)
        --putStrLn $ "- TestFlagZ (" <> show z <> ") -> " <> show pred
        k s pred

      --Now{} -> k s ticks

      Out port byte -> do
        putStrLn $ show ("OUT",port,byte)
        k s ()

      InstructionCycle eff -> do
        let s0 = s

        when (traceOn && splitTrace) $
          putStrLn (ljust cpuCol (prettyTicks s) ++ show cpu)

        run s eff $ \s@State{icount,ticks,cpu} (instruction,n) -> do
          let s1 = s { icount = icount + 1, ticks = ticks + fromIntegral n }

          when traceOn $
            if splitTrace
            then putStrLn (prettyStep s0 instruction)
            else putStrLn (ljust cpuCol (prettyStep s0 instruction) ++ show cpu)

          printWhenNewFrame s0 s1
          k s1 ()

            where
              splitTrace = False
              cpuCol = 60
              traceOn = True

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

data State = State
  { ticks :: Ticks -- cycle count
  , icount :: Int -- instruction count
  , fcount :: Int -- frame count
  , cpu :: Cpu Byte
  , mem :: Mem
  }

state0 :: Mem -> State
state0 mem = State
  { ticks = 0
  , icount = 0
  , fcount = 0
  , cpu = Cpu.init (Byte 0)
  , mem
  }

programCounter :: State -> Addr
programCounter State{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}


printWhenNewFrame :: State -> State -> IO ()
printWhenNewFrame s0 s1 = do
  let State{ticks=ticks0} = s0
  let State{ticks=ticks1} = s1
  let f0 = unTicks ticks0 `div` cyclesPerFrame
  let f1 = unTicks ticks1 `div` cyclesPerFrame
  let yes = f1 > f0
  when yes $ do
    let State{mem} = s1
    let pixs = onPixels (getDisplayFromMem mem)
    let _nCyclesLate = unTicks ticks1 `mod` cyclesPerFrame
    let _nFramesSkipped = f1 - f0 - 1
    putStrLn $ unwords
      [ prettyTicks s1
      , printf "FRAME{%d}" f1
      , printf "#onPixels = %d" (length pixs)
      , printf "(cycles-late=%d)" _nCyclesLate
      , printf "(frame-skipped=%d)" _nFramesSkipped
       --, show (f0,f1)
       --, show (ticks0,ticks1)
      ]
    where cyclesPerFrame = 1000
    --where cyclesPerFrame = 33333


prettyStep :: State -> Instruction Byte -> String
prettyStep s instruction = do
  let pc = programCounter s
  unwords [ prettyTicks s, show pc, ":", show instruction]

prettyTicks :: State -> String
prettyTicks State{ticks,icount} =
  unwords [ printf "(%5d)" icount, show ticks ]


data OnPixel = OnPixel -- { x :: Int, y :: Int }

data Display = Display { onPixels :: [OnPixel] }

getDisplayFromMem :: Mem -> Display
getDisplayFromMem _ =
  Display { onPixels = [OnPixel,OnPixel] }


