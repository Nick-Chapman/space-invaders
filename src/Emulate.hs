
module Emulate (
  emulate, Emulation(..), EmuState(..), Ticks(..),
  ) where

import Data.Bits

import Addr (Addr(..))
import Byte (Byte(..),adc)
import Cpu (Cpu)
import Effect (Eff(..))
import Semantics (fetchDecodeExec)
import InstructionSet (Instruction,decode)
import Mem (Mem)
import Phase (Phase)
import Text.Printf (printf)
import qualified Addr (fromHiLo,toHiLo,bump,add)
import qualified Cpu (init,get,set,getFlag,setFlag)
import qualified Mem (read,write)
import qualified Phase (Byte,Addr,Ticks,Bit)


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


data Emulation
  = CrashDecode EmuState Byte
  | EmuStep
    { pre :: EmuState
    , instruction :: Instruction Byte
    , post :: EmuState
    , continue :: IO Emulation
    }

data EmuState = EmuState
  { ticks :: Ticks -- cycle count
  , icount :: Int -- instruction count
  , fcount :: Int -- frame count
  , cpu :: Cpu EmuTime
  , mem :: Mem
  , interrupts_enabled :: Bool
  , nextWakeup :: Ticks
  }

state0 :: Mem -> EmuState
state0 mem = EmuState
  { ticks = 0
  , icount = 0
  , fcount = 0
  , cpu = Cpu.init (Byte 0) (Bit False)
  , mem
  , interrupts_enabled = False
  , nextWakeup = halfFrameTicks
  }

theSemantics :: Eff p ()
theSemantics = loop
  where
    loop = do
      fetchDecodeExec
      loop

emulate :: Mem -> IO Emulation
emulate mem0 = run (state0 mem0) theSemantics $ \_ () -> error "unexpected emulation end"
  where
    run :: EmuState -> Eff EmuTime a -> (EmuState -> a -> IO Emulation) -> IO Emulation
    run s@EmuState{cpu,mem} eff k = case eff of
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

      Decode byte -> do
        case decode byte of
          Just op -> k s op
          Nothing -> return (CrashDecode s byte)

      MakeByte w -> k s (Byte w)
      Increment b -> k s (b + 1)
      Decrement b -> k s (b - 1)

      AddWithCarry (Bit cin) v1 v2 -> do
        let (v,cout) = Byte.adc cin v1 v2
        k s (v, Bit cout)

      Complement b -> k s (complement b)
      Flip (Bit bool) -> k s (Bit (not bool))

      AndB b1 b2 -> k s (b1 .&. b2)
      OrB b1 b2 -> k s (b1 .|. b2)
      XorB b1 b2 -> k s (b1 `xor` b2)

      -- Word (Address) ops
      Add16 a1 a2 -> k s (Addr.add a1 a2) -- TODO: dont loose carry

      -- TODO: Z should be in position 6, not 7 !
      SelectBit70 byte -> k s (Bit (byte `testBit` 7), Bit (byte `testBit` 0))
      ByteFromBit70 (Bit z, Bit cy) -> k s ((if z then 128 else 0) + (if cy then 1 else 0))

      GetFlag flag -> k s (Cpu.getFlag cpu flag)
      SetFlag flag bit -> k s { cpu = Cpu.setFlag cpu flag bit} ()
      IsZero byte -> k s (Bit (byte == 0))
      TestBit (Bit bool) -> k s bool
      MakeBit (bool) -> k s (Bit bool)

      RotateRightThroughCarry (Bit cin,before) -> do -- RAR
        let cout = before `testBit` 0
        let after = (if cin then 128 else 0) + shiftR before 1
        k s (after,Bit cout)

      RotateLeftThroughCarry (Bit cin,before) -> do  -- RAL
        let cout = before `testBit` 7
        let after = shiftL before 1 + (if cin then 1 else 0)
        k s (after,Bit cout)

      RotateRight before -> do -- RRC
        let bit = before `testBit` 0
        let after = (if bit then 128 else 0) + shiftR before 1
        k s (after,Bit bit)

      RotateLeft before -> do -- RLC
        let bit = before `testBit` 7
        let after = shiftL before 1 + (if bit then 1 else 0)
        k s (after,Bit bit)

      Out port byte -> do
        case port of
          2 -> do
            print ("OUT-2",byte) -- TODO: shift register result offset (bits 0,1,2)
          3 -> do
            --print ("OUT-3",byte) -- sound related
            return ()
          4 -> do
            print ("OUT-4",byte) -- TODO: fill shift register
          5 -> do
            --print ("OUT-5",byte) -- sound related
            return ()
          6 -> do
            return ()
          _ -> do
            error $ show ("OUT",port,byte)
        k s ()

      In port -> do
        let byte = case port of
              1 -> 0 -- 1 is recomended in emulator101 for attract mode only
              2 -> 0
              3 -> 0 -- TODO: shift register result here
              _ -> error $ show ("IN",port)
        --putStrLn $ show ("IN",port,byte)
        k s byte

      EnableInterrupts -> k s { interrupts_enabled = True } ()
      DisableInterrupts -> k s { interrupts_enabled = False } ()
      AreInterruptsEnabled -> k s (interrupts_enabled s)
      TimeToWakeup -> case timeToWakeup s of
        Nothing -> k s False
        Just s -> k s True
      GetInterruptInstruction -> k s (interruptInstruction s)

      InstructionCycle eff -> do
        let pre = s
        run s eff $ \s (instruction,n) -> do
          let post = advance (Ticks n) s
          let continue = k post ()
          let step = EmuStep { pre, instruction, post, continue }
          return step


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
