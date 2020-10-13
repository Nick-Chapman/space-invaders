
module Emulate (emulate) where

import Control.Monad (when)
import Data.Bits

import Addr (Addr(..))
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
import qualified Cpu (init,get,set,getFlag,setFlag)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr,Ticks,Bit)


-- | Ticks of the 2 MHz clock
newtype Ticks = Ticks { unTicks :: Int } deriving (Num)

instance Show Ticks where show = printf "[%6d]" . unTicks


data EmuTime -- At Emulation type we have concrete Bytes

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr
  type Ticks EmuTime = Ticks
  type Bit EmuTime = Bit


newtype Bit = Bit Bool

instance Show Bit where show (Bit b) = if b then "1" else "0"


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

emulate :: Bool -> Mem -> IO ()
emulate traceOn mem0 = run (state0 mem0) theSemantics $ \_ -> return
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
            putStrLn (prettyTicks s <> " " <> show pc <> " : " <> show byte)
            error $ "Decode: " <> show byte

      -- Byte ops
      Decrement b -> k s (b - 1)
      AddB b1 b2 -> k s (b1 + b2)
      SubtractB b1 b2 -> k s (b1 - b2)
      AndB b1 b2 -> k s (b1 .&. b2)
      XorB b1 b2 -> k s (b1 `xor` b2)

      -- Word (Address) ops
      Add16 a1 a2 -> k s (Addr.add a1 a2) -- TODO: dont loose carry

      SelectBit0 byte -> k s (Bit (byte `testBit` 0))
      ByteFromBit0 (Bit b) -> k s (if b then 1 else 0)
      GetFlag flag -> k s (Cpu.getFlag cpu flag)
      SetFlag flag bit -> k s { cpu = Cpu.setFlag cpu flag bit} ()
      IsZero byte -> k s (Bit (byte == 0))
      TestBit (Bit bool) -> k s bool

      RotateRight (Bit bit,byte) -> do
        let bit' = byte `testBit` 0
        let byte' = (if bit then 128 else 0) + shiftR byte 1
        k s (byte',Bit bit')

      Out port byte -> do
        let _ = putStrLn $ show ("OUT",port,byte)
        k s ()

      EnableInterrupts -> do
        -- TODO: add bit in cpu to record enabled/disabled status
        k s ()

      --Now{} -> k s ticks

      InstructionCycle eff -> do
        let s0 = s

        when (traceOn && splitTrace) $
          putStrLn (ljust cpuCol (prettyTicks s) ++ show cpu)

        run s eff $ \s@State{icount,ticks,cpu} (instruction,n) -> do
          let s1 = s { icount = icount + 1, ticks = ticks + fromIntegral n }

          when (icount > 42050) $ error "STOP"

          when traceOn $
            if splitTrace
            then putStrLn (prettyStep s0 instruction)
            else putStrLn (ljust cpuCol (prettyStep s0 instruction) ++ show cpu)

          printWhenNewFrame s0 s1
          k s1 ()

            where
              splitTrace = False
              cpuCol = 60

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

data State = State
  { ticks :: Ticks -- cycle count
  , icount :: Int -- instruction count
  , fcount :: Int -- frame count
  , cpu :: Cpu EmuTime
  , mem :: Mem
  }

state0 :: Mem -> State
state0 mem = State
  { ticks = 0
  , icount = 0
  , fcount = 0
  , cpu = Cpu.init (Byte 0) (Bit False)
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
      --, printf "(cycles-late=%d)" _nCyclesLate
      --, printf "(frame-skipped=%d)" _nFramesSkipped
      --, show (f0,f1)
      --, show (ticks0,ticks1)
      ]
    --where cyclesPerFrame = 1000
    where cyclesPerFrame = 33333


prettyStep :: State -> Instruction Byte -> String
prettyStep s instruction = do
  let pc = programCounter s
  unwords [ prettyTicks s, show pc, ":", show instruction]

prettyTicks :: State -> String
prettyTicks State{ticks,icount} =
  unwords [ printf "(%5d)" icount, show ticks ]


data OnPixel = OnPixel { x :: Int, y :: Int }

data Display = Display { onPixels :: [OnPixel] }

getDisplayFromMem :: Mem -> Display
getDisplayFromMem mem = do
  Display
    [ OnPixel {x, y}
    | x :: Int <- [0..223]
    , yByte <- [0..31]
    , let byte = Mem.read mem (Addr (fromIntegral (0x2400 + x * 32 + yByte)))
    , yBit <- [0..7]
    , byte `testBit` yBit
    , let y  = 8 * yByte + yBit
    ]
