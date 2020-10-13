
module Effect (Eff(..)) where

import Control.Monad (ap,liftM)
import Cpu (Reg,Flag)
import Data.Word8 (Word8)
import HiLo (HiLo(..))
import Phase (Byte,Addr,Bit)
import InstructionSet (Op,Instruction)

-- | The Effect type, constructed when executing instructions

data Eff p a where
  Ret :: a -> Eff p a
  Bind :: Eff p a -> (a -> Eff p b) -> Eff p b
  GetReg :: Reg -> Eff p (Byte p)
  SetReg :: Reg -> Byte p -> Eff p ()
  ReadMem :: Addr p -> Eff p (Byte p)
  WriteMem :: Addr p -> Byte p -> Eff p ()
  SplitAddr :: Addr p -> Eff p (HiLo (Byte p))
  MakeAddr :: HiLo (Byte p) -> Eff p (Addr p)
  OffsetAddr :: Int -> Addr p -> Eff p (Addr p)
  Decode :: (Addr p, Byte p) -> Eff p Op
  Decrement :: Byte p -> Eff p (Byte p)

  MakeByte :: Word8 -> Eff p (Byte p)
  AddB :: Byte p -> Byte p -> Eff p (Byte p)
  SubtractB :: Byte p -> Byte p -> Eff p (Byte p)
  AndB :: Byte p -> Byte p -> Eff p (Byte p)
  XorB :: Byte p -> Byte p -> Eff p (Byte p)
  Add16 :: Addr p -> Addr p -> Eff p (Addr p)

  SelectBit70 :: Byte p -> Eff p (Bit p, Bit p)
  ByteFromBit70 :: (Bit p, Bit p) -> Eff p (Byte p)

  GetFlag :: Flag -> Eff p (Bit p)
  SetFlag :: Flag -> Bit p -> Eff p ()
  IsZero :: Byte p -> Eff p (Bit p)
  TestBit :: Bit p -> Eff p Bool

  RotateRight :: (Bit p,Byte p) -> Eff p (Byte p,Bit p)

  Out :: Byte p -> Byte p -> Eff p ()

  EnableInterrupts :: Eff p ()
  DisableInterrupts :: Eff p ()
  AreInterruptsEnabled :: Eff p Bool
  TimeToWakeup :: Eff p Bool
  GetInterruptInstruction :: Eff p (Byte p)

  InstructionCycle :: Eff p (Instruction (Byte p), Int) -> Eff p ()

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
