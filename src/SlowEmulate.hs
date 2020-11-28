
module SlowEmulate (
  Ticks(..),
  prettyPrefix,
  EmuState(..), initState,
  CB(..),
  emulate,
  Bit(..),
  ) where

import Addr (Addr(..),addCarryOut)
import Buttons (Buttons)
import Byte (Byte(..),addWithCarry)
import Cpu (Cpu,Reg(PCL,PCH))
import Data.Bits
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Instruction,decode)
import Mem (Mem)
import Phase (Phase)
import Rom (Rom)
import Shifter (Shifter)
import Sounds (soundControl)
import Text.Printf (printf)
import qualified Addr (fromHiLo,toHiLo,bump)
import qualified Buttons (get)
import qualified Byte (toUnsigned)
import qualified Cpu (init,get,set,getFlag,setFlag)
import qualified Mem (init,read,write)
import qualified Phase (Byte,Addr,Bit)
import qualified Semantics (fetchDecodeExec,decodeExec)
import qualified Shifter (init,get,set)
import qualified Sounds (Playing,initPlaying)


-- | Ticks of the 2 MHz clock
newtype Ticks = Ticks { unTicks :: Int } deriving (Eq,Ord,Num)

instance Show Ticks where show = printf "[%d]" . unTicks


data EmuTime -- At Emulation type we have concrete Bytes

instance Phase EmuTime where
  type Byte EmuTime = Byte
  type Addr EmuTime = Addr
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
  , shifter :: Shifter EmuTime
  , playing :: Sounds.Playing
  }

initState :: Rom -> IO EmuState
initState rom = return $ EmuState
  { ticks = 0
  , icount = 0
  , cpu = Cpu.init (Byte 0) (Bit False)
  , mem = Mem.init rom
  , interrupts_enabled = False

  , nextWakeup = halfFrameTicks
  , shifter = Shifter.init (Byte 0)
  , playing = Sounds.initPlaying
  }

instance Show EmuState where
  show EmuState{cpu,shifter=_} =
    unwords [ show cpu
            -- , show shifter
            ]


data CB = CB
  { traceI :: Maybe (EmuState -> Instruction Byte -> IO ())
  }

emulate :: CB -> Buttons -> EmuState -> IO EmuState
emulate cb buttons s@EmuState{interrupts_enabled} = do
  case timeToWakeup s of
    Just s
      | interrupts_enabled -> do
          let s2 = s { interrupts_enabled = False }
          let byte = interruptInstruction s2
          emulateS cb (Semantics.decodeExec byte) buttons s2
      | otherwise ->
        emulateS cb Semantics.fetchDecodeExec buttons s
    Nothing ->
      emulateS cb Semantics.fetchDecodeExec buttons s


emulateS :: CB -> Eff EmuTime () -> Buttons -> EmuState -> IO EmuState
emulateS CB{traceI} semantics buttons s0 = do
  run s0 semantics $ \post () -> do
    return post

  where

    crash :: String -> a
    crash message = do error ("*crash*\n" <> prettyPrefix s0 message)

    run :: EmuState -> Eff EmuTime a -> (EmuState -> a -> IO EmuState) -> IO EmuState
    run s@EmuState{cpu,shifter,mem,playing} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k

      GetReg r -> k s (Cpu.get cpu r)
      SetReg r b -> k s { cpu = Cpu.set cpu r b} ()
      GetFlag flag -> k s (Cpu.getFlag cpu flag)
      SetFlag flag bit -> k s { cpu = Cpu.setFlag cpu flag bit} ()

      GetShifterReg r -> k s (Shifter.get shifter r)
      SetShifterReg r b -> k s { shifter = Shifter.set shifter r b} ()

      ReadMem a -> k s (Mem.read mem a)
      WriteMem a b -> k s { mem = Mem.write mem a b } ()

      EnableInterrupts -> k s { interrupts_enabled = True } ()
      DisableInterrupts -> k s { interrupts_enabled = False } ()

      Decode byte -> k s (decode byte)
      MarkReturnAddress {} -> k s ()
      TraceInstruction i -> do
        case traceI of
          Nothing -> return ()
          Just tr -> tr s i
        k s ()

      Advance n -> k (advance (Ticks n) s) ()

      MakeBit (bool) -> k s (Bit bool)
      Flip (Bit bool) -> k s (Bit (not bool))
      AndBit (Bit b1) (Bit b2) -> k s (Bit (b1 && b2))
      OrBit (Bit b1) (Bit b2) -> k s (Bit (b1 || b2))
      CaseBit (Bit bool) -> k s bool

      MakeByte w -> k s (Byte w)
      ShiftRight byte offset -> k s (byte `shiftR` (Byte.toUnsigned offset))
      ShiftLeft byte offset -> k s (byte `shiftL` (Byte.toUnsigned offset))
      Complement b -> k s (complement b)
      AndB b1 b2 -> k s (b1 .&. b2)
      OrB b1 b2 -> k s (b1 .|. b2)
      XorB b1 b2 -> k s (b1 `xor` b2)
      Ite (Bit i) t e -> k s (if i then t else e)
      AddWithCarry (Bit cin) v1 v2 -> do
        let (v,cout) = Byte.addWithCarry cin v1 v2
        k s (v, Bit cout)
      IsSigned byte -> k s (Bit (byte `testBit` 7))
      IsZero byte -> k s (Bit (byte == 0))
      IsParity byte -> do k s (Bit (parity  byte))
      TestBit byte i -> k s (Bit (byte `testBit` i))
      UpdateBit byte i (Bit bool) -> k s ((if bool then setBit else clearBit) byte i)
      CaseByte (Byte word) -> k s word

      MakeAddr hilo -> k s (Addr.fromHiLo hilo)
      SplitAddr a -> k s (Addr.toHiLo a)
      OffsetAddr n a -> k s (Addr.bump a n)
      Add16 w1 w2 -> do
        let (w, cout) = Addr.addCarryOut w1 w2
        k s (w, Bit cout)

      UnknownInput n -> crash $ "unknown input: " ++ show n
      UnknownOutput n -> crash $ "unknown output: " ++ show n
      GetButton but -> k s (Bit (Buttons.get but buttons))
      SoundControl sound (Bit bool) -> do
        k s { playing = soundControl bool playing sound } ()


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
halfFrameTicks = Ticks (2000000 `div` 120) - n -- Experiment with reducing this value.
  -- turns out that even a reduction of just 1000 cycles
  -- will cause the game to hang after the "P" of "PLAY" is displayed
  where n = 0 --1000

interruptInstruction :: EmuState -> Byte
interruptInstruction EmuState{ticks} = do
  let mid = (unTicks ticks `div` unTicks halfFrameTicks) `mod` 2 == 1
  if mid then 0xCF else 0xD7

timeToWakeup :: EmuState -> Maybe EmuState
timeToWakeup s@EmuState{ticks,nextWakeup} = do
  if ticks < nextWakeup
    then Nothing
    else do
    Just s { nextWakeup =
             Ticks ((unTicks ticks `div` unTicks halfFrameTicks) + 1) * halfFrameTicks
           }
