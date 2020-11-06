
module Effect (Eff(..)) where

import Control.Monad (ap,liftM)
import Data.Word8 (Word8)

import Buttons (Buttons)
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
  ReadMem :: Addr p -> Eff p (Byte p)
  WriteMem :: Addr p -> Byte p -> Eff p ()
  SplitAddr :: Addr p -> Eff p (HiLo (Byte p))
  MakeAddr :: HiLo (Byte p) -> Eff p (Addr p)
  OffsetAddr :: Int -> Addr p -> Eff p (Addr p)
  Decode :: Byte p -> Eff p Op

  MakeByte :: Word8 -> Eff p (Byte p)
  AddWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)

  Complement :: Byte p -> Eff p (Byte p)
  Flip :: Bit p -> Eff p (Bit p)

  AndB :: Byte p -> Byte p -> Eff p (Byte p)
  OrB :: Byte p -> Byte p -> Eff p (Byte p)
  XorB :: Byte p -> Byte p -> Eff p (Byte p)
  Add16 :: Addr p -> Addr p -> Eff p (Addr p, Bit p)

  GetFlag :: Flag -> Eff p (Bit p)
  SetFlag :: Flag -> Bit p -> Eff p ()

  IsSigned :: Byte p -> Eff p (Bit p)
  IsZero :: Byte p -> Eff p (Bit p)
  IsParity :: Byte p -> Eff p (Bit p)

  CaseBit :: Bit p -> Eff p Bool
  MakeBit :: Bool -> Eff p (Bit p)

  -- TODO: deprecate use of rotate ops, and instead use shift
  RotateRight :: Byte p -> Eff p (Byte p)
  RotateLeft :: Byte p -> Eff p (Byte p)

  ShiftRight :: Byte p -> Byte p -> Eff p (Byte p)
  ShiftLeft :: Byte p -> Byte p -> Eff p (Byte p)

  AndBit :: Bit p -> Bit p -> Eff p (Bit p)
  OrBit :: Bit p -> Bit p -> Eff p (Bit p)

  Ite :: Bit p -> Byte p -> Byte p -> Eff p (Byte p)

  EnableInterrupts :: Eff p ()
  DisableInterrupts :: Eff p ()
  AreInterruptsEnabled :: Eff p Bool
  TimeToWakeup :: Eff p Bool
  GetInterruptInstruction :: Eff p (Byte p)

  UnknownInput :: Word8 -> Eff p (Byte p)
  UnknownOutput :: Word8 -> Eff p ()

  GetButtons :: Eff p Buttons
  DispatchByte :: Byte p -> Eff p Word8

  SoundControl :: Sound -> Bit p -> Eff p ()

  TestBit :: Byte p -> Int -> Eff p (Bit p)
  UpdateBit :: Byte p -> Int -> Bit p -> Eff p (Byte p)

  FillShiftRegister :: Byte p -> Eff p ()
  SetShiftRegisterOffset :: Byte p -> Eff p ()
  GetShiftRegisterAtOffset :: Eff p (Byte p)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
