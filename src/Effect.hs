
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
  Decode :: Byte p -> Eff p Op
  Increment :: Byte p -> Eff p (Byte p)
  Decrement :: Byte p -> Eff p (Byte p)

  MakeByte :: Word8 -> Eff p (Byte p)
  AddWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
  Complement :: Byte p -> Eff p (Byte p)
  Flip :: Bit p -> Eff p (Bit p)

  AndB :: Byte p -> Byte p -> Eff p (Byte p)
  OrB :: Byte p -> Byte p -> Eff p (Byte p)
  XorB :: Byte p -> Byte p -> Eff p (Byte p)
  Add16 :: Addr p -> Addr p -> Eff p (Addr p, Bit p)

  SelectSZC :: Byte p -> Eff p (Bit p, Bit p, Bit p)
  ByteFromSZC :: (Bit p, Bit p, Bit p) -> Eff p (Byte p)

  GetFlag :: Flag -> Eff p (Bit p)
  SetFlag :: Flag -> Bit p -> Eff p ()
  IsSigned :: Byte p -> Eff p (Bit p)
  IsZero :: Byte p -> Eff p (Bit p)
  TestBit :: Bit p -> Eff p Bool
  MakeBit :: Bool -> Eff p (Bit p)

  RotateRightThroughCarry :: (Bit p,Byte p) -> Eff p (Byte p,Bit p)
  RotateLeftThroughCarry :: (Bit p,Byte p) -> Eff p (Byte p,Bit p)

  RotateRight :: Byte p -> Eff p (Byte p,Bit p)
  RotateLeft :: Byte p -> Eff p (Byte p,Bit p)

  Out :: Byte p -> Byte p -> Eff p ()
  In :: Byte p -> Eff p (Byte p)

  EnableInterrupts :: Eff p ()
  DisableInterrupts :: Eff p ()
  AreInterruptsEnabled :: Eff p Bool
  TimeToWakeup :: Eff p Bool
  GetInterruptInstruction :: Eff p (Byte p)

  Unimplemented :: String -> Eff p ()

  InstructionCycle :: Eff p (Instruction (Byte p), Int) -> Eff p ()

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
