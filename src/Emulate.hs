
module Emulate (
  Ticks(..),
  prettyPrefix,
  EmuState(..), initState,
  EmuStep(..), emulate,
  Bit(..),
  ) where

import Data.Bits
import Text.Printf (printf)

import Addr (Addr(..),addCarryOut)
import Buttons (Buttons)
import Byte (Byte(..),addWithCarry,decimalAdjust)
import Cpu (Cpu,Reg(PCL,PCH))
import Effect (Eff(..))
import Semantics (fetchDecodeExec)
import HiLo (HiLo(..))
import InstructionSet (Instruction,decode)
import Mem (Mem)
import Phase (Phase)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Cpu (init,get,set,getFlag,setFlag)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr,Ticks,Bit)
import qualified Sounds (Playing,initPlaying,soundOn,soundOff)


-- | Ticks of the 2 MHz clock
newtype Ticks = Ticks { unTicks :: Int } deriving (Eq,Ord,Num)

instance Show Ticks where show = printf "[%d]" . unTicks


data EmuTime -- At Emulation type we have concrete Bytes

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr
  type Ticks EmuTime = Ticks
  type Bit EmuTime = Bit


newtype Bit = Bit Bool

instance Show Bit where show (Bit b) = if b then "1" else "0"


data EmuState = EmuState
  { ticks :: Ticks -- cycle count
  , icount :: Int -- instruction count
  , cpu :: Cpu EmuTime
  , mem :: Mem
  , interrupts_enabled :: Bool
  , nextWakeup :: Ticks
  , shifter :: Shifter
  , playing :: Sounds.Playing
  }

initState :: Mem -> EmuState
initState mem = EmuState
  { ticks = 0
  , icount = 0
  , cpu = Cpu.init (Byte 0) (Bit False)
  , mem
  , interrupts_enabled = False
  , nextWakeup = halfFrameTicks
  , shifter = shifter0
  , playing = Sounds.initPlaying
  }

instance Show EmuState where
  show EmuState{cpu,shifter} =
    unwords [ show cpu, show shifter ]


data EmuStep = EmuStep
    { instruction :: Instruction Byte
    , post :: EmuState
    }

emulate :: Buttons -> EmuState -> IO EmuStep
emulate buttons s0 =
  run s0 fetchDecodeExec $ \s (instruction,n) -> do
  return $ EmuStep { instruction, post = advance (Ticks n) s }
  where

    crash :: String -> a
    crash message = do error ("*crash*\n" <> prettyPrefix s0 message)

    run :: EmuState -> Eff EmuTime a -> (EmuState -> a -> IO EmuStep) -> IO EmuStep
    run s@EmuState{cpu,mem,playing} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k
      GetReg r -> k s (Cpu.get cpu r)
      SetReg r b -> k s { cpu = Cpu.set cpu r b} ()
      ReadMem a -> do
        let b = Mem.read crash mem a
        --putStrLn $ "- ReadMem (" <> show a <> ") --> " <> show b
        k s b
      WriteMem a b -> do
        --putStrLn $ "- WriteMem (" <> show a <> ") = " <> show b
        k s { mem = Mem.write crash mem a b } ()
      SplitAddr a -> k s (Addr.toHiLo a)
      MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      OffsetAddr n a -> k s (Addr.bump a n)

      Decode byte -> k s (decode byte)

      MakeByte w -> k s (Byte w)

      AddWithCarry (Bit cin) v1 v2 -> do
        let (v,cout) = Byte.addWithCarry cin v1 v2
        k s (v, Bit cout)

      -- TODO: move the decimal-adjust implementation into Semantics
      DecimalAdjust (Bit auxIn) (Bit cin) byteIn -> do
        let (byteOut,auxOut,cout) = Byte.decimalAdjust auxIn cin byteIn
        k s (byteOut, Bit auxOut, Bit cout)

      Complement b -> k s (complement b)
      Flip (Bit bool) -> k s (Bit (not bool))

      AndB b1 b2 -> k s (b1 .&. b2)
      OrB b1 b2 -> k s (b1 .|. b2)
      XorB b1 b2 -> k s (b1 `xor` b2)

      Add16 w1 w2 -> do
        let (w, cout) = Addr.addCarryOut w1 w2
        k s (w, Bit cout)

      GetFlag flag -> k s (Cpu.getFlag cpu flag)
      SetFlag flag bit -> k s { cpu = Cpu.setFlag cpu flag bit} ()

      IsSigned byte -> k s (Bit (byte `testBit` 7))
      IsZero byte -> k s (Bit (byte == 0))
      IsParity byte -> do k s (Bit (parity  byte))

      CaseBit (Bit bool) -> k s bool
      MakeBit (bool) -> k s (Bit bool)

      RotateRight before -> do
        let bit = before `testBit` 0
        let after = (if bit then 128 else 0) + shiftR before 1
        k s after

      RotateLeft before -> do
        let bit = before `testBit` 7
        let after = shiftL before 1 + (if bit then 1 else 0)
        k s after

      EnableInterrupts -> k s { interrupts_enabled = True } ()
      DisableInterrupts -> k s { interrupts_enabled = False } ()
      AreInterruptsEnabled -> k s (interrupts_enabled s)
      TimeToWakeup -> case timeToWakeup s of
        Nothing -> k s False
        Just s -> k s True
      GetInterruptInstruction -> k s (interruptInstruction s)

      UnknownInput n -> do
        crash $ "unknown input: " ++ show n

      UnknownOutput n -> do
        crash $ "unknown input: " ++ show n

      GetButtons -> do
        k s buttons

      DispatchByte (Byte word) ->
        k s word

      TestBit byte i -> k s (Bit (byte `testBit` i))
      UpdateBit byte i (Bit bool) -> k s ((if bool then setBit else clearBit) byte i)

      SoundControl sound (Bit bool) -> do
        k s { playing = (if bool then Sounds.soundOn else Sounds.soundOff) playing sound } ()

      FillShiftRegister byte -> do
        k s { shifter = fillShiftRegister (shifter s) byte } ()

      SetShiftRegisterOffset byte -> do
        k s { shifter = setShiftRegisterOffset (shifter s) byte } ()

      GetShiftRegisterAtOffset -> do
        let res = getShiftRegisterAtOffset (shifter s)
        k s res


parity :: Byte -> Bool
parity byte = length [ () | i <- [0..7], byte `testBit` i ] `mod` 2 == 0

prettyTicks :: EmuState -> String
prettyTicks EmuState{ticks,icount} =
  unwords [ printf "%8d" icount, rjust 11 (show ticks) ]

rjust :: Int -> String -> String
rjust n s = take (max 0 (n - length s)) (repeat ' ') <> s

programCounter :: EmuState -> Addr
programCounter EmuState{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}

prettyPrefix :: EmuState -> String -> String
prettyPrefix s message = do
  let pc = programCounter s
  unwords [ prettyTicks s , show pc , ":", message ]


advance :: Ticks -> EmuState -> EmuState
advance n s@EmuState{icount,ticks} =
  s { icount = icount + 1, ticks = ticks + n }

halfFrameTicks :: Ticks
halfFrameTicks = Ticks (2000000 `div` 120)

interruptInstruction :: EmuState -> Byte
interruptInstruction EmuState{ticks} = do
  let mid = (unTicks ticks `mod` unTicks halfFrameTicks) `mod` 2 == 1
  if mid then 0xCF else 0xD7

timeToWakeup :: EmuState -> Maybe EmuState
timeToWakeup s@EmuState{ticks,nextWakeup} = do
  if ticks < nextWakeup
    then Nothing
    else do
    Just s { nextWakeup =
             Ticks ((unTicks ticks `div` unTicks halfFrameTicks) + 1) * halfFrameTicks
           }

data Shifter = Shifter
  { offset :: Byte
  , hi :: Byte
  , lo :: Byte
  }

instance Show Shifter where
  show Shifter{offset,hi,lo} =
    "shifter(" <> show offset <> "," <> show hi <> show lo <> ")"

shifter0 :: Shifter
shifter0 = Shifter
  { offset = Byte 0
  , hi = Byte 0
  , lo = Byte 0
  }

fillShiftRegister :: Shifter -> Byte -> Shifter
fillShiftRegister shifter@Shifter{hi} byte = shifter { hi = byte, lo = hi }

setShiftRegisterOffset :: Shifter -> Byte -> Shifter
setShiftRegisterOffset shifter byte = shifter { offset = byte}

getShiftRegisterAtOffset :: Shifter -> Byte
getShiftRegisterAtOffset Shifter{offset,lo,hi} = do
  let off :: Int = fromIntegral (unByte offset)
  if (off < 0 || off > 7) then error $ "off=" <> show off else
    shiftL hi off .|. shiftR lo (8-off)
