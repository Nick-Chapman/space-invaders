
module Effect (Eff(..)) where

import Control.Monad (ap,liftM)
import Cpu (Reg(..))
import HiLo (HiLo(..))
import Phase (Byte,Addr)
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
  InstructionCycle :: Eff p (Instruction (Byte p)) -> Eff p ()

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
