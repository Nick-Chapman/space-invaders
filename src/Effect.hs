
module Effect (Eff(..)) where

import Control.Monad (ap,liftM)
import Data.Word8 (Word8)

import Buttons (But)
import Cpu (Reg,Flag)
import HiLo (HiLo(..))
import InstructionSet (Op)
import Phase (Byte,Addr,Bit)
import Sounds (Sound)

-- | The Effect type, constructed when executing instructions

data Eff p a where
  Ret :: a -> Eff p a
  Bind :: Eff p a -> (a -> Eff p b) -> Eff p b

  GetReg :: Reg -> Eff p (Byte p)
  SetReg :: Reg -> Byte p -> Eff p ()
  GetFlag :: Flag -> Eff p (Bit p)
  SetFlag :: Flag -> Bit p -> Eff p ()

  ReadMem :: Addr p -> Eff p (Byte p)
  WriteMem :: Addr p -> Byte p -> Eff p ()

  -- TODO: treat shift registers more directly (like cpu regs) & use shift ops directly
  FillShiftRegister :: Byte p -> Eff p ()
  SetShiftRegisterOffset :: Byte p -> Eff p ()
  GetShiftRegisterAtOffset :: Eff p (Byte p)

  -- TODO: rethink the interrupt timing & triggering, plus add effects to Advance the cycle-count
  -- TODO: treatthe is_enabled like a cpu flag
  EnableInterrupts :: Eff p ()
  DisableInterrupts :: Eff p ()
  AreInterruptsEnabled :: Eff p Bool
  TimeToWakeup :: Eff p Bool

  MarkReturnAddress :: Addr p -> Eff p ()

  GetInterruptInstruction :: Eff p (Byte p)
  Decode :: Byte p -> Eff p Op

  MakeBit :: Bool -> Eff p (Bit p)
  Flip :: Bit p -> Eff p (Bit p)
  AndBit :: Bit p -> Bit p -> Eff p (Bit p)
  OrBit :: Bit p -> Bit p -> Eff p (Bit p)
  CaseBit :: Bit p -> Eff p Bool -- rename DispatchBit

  MakeByte :: Word8 -> Eff p (Byte p)
  ShiftRight :: Byte p -> Byte p -> Eff p (Byte p)
  ShiftLeft :: Byte p -> Byte p -> Eff p (Byte p)
  Complement :: Byte p -> Eff p (Byte p)
  AndB :: Byte p -> Byte p -> Eff p (Byte p)
  OrB :: Byte p -> Byte p -> Eff p (Byte p)
  XorB :: Byte p -> Byte p -> Eff p (Byte p)
  Ite :: Bit p -> Byte p -> Byte p -> Eff p (Byte p)
  AddWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
  IsSigned :: Byte p -> Eff p (Bit p)
  IsZero :: Byte p -> Eff p (Bit p)
  IsParity :: Byte p -> Eff p (Bit p)
  TestBit :: Byte p -> Int -> Eff p (Bit p)
  UpdateBit :: Byte p -> Int -> Bit p -> Eff p (Byte p)
  DispatchByte :: Byte p -> Eff p Word8

  MakeAddr :: HiLo (Byte p) -> Eff p (Addr p)
  SplitAddr :: Addr p -> Eff p (HiLo (Byte p))
  OffsetAddr :: Int -> Addr p -> Eff p (Addr p)
  Add16 :: Addr p -> Addr p -> Eff p (Addr p, Bit p)

  UnknownInput :: Word8 -> Eff p (Byte p)
  UnknownOutput :: Word8 -> Eff p ()
  GetButton :: But -> Eff p (Bit p)
  SoundControl :: Sound -> Bit p -> Eff p ()


instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
