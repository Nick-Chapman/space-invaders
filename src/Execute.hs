
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Execute (execute0,execute1,execute2,Flow(..)) where

import Cpu (Reg(..))
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op0(..),Op1(..),Op2(..))
import Phase (Addr,Byte)

data Flow p = Next | Jump (Addr p)

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return Next

execute1 :: Op1 -> Byte p -> Eff p (Flow p)
execute1 op1 b1 = case op1 of
  MVI_B -> do
    SetReg RegB b1
    return Next

execute2 :: Op2 -> (Byte p, Byte p) -> Eff p (Flow p)
execute2 op2 (lo,hi) = case op2 of
  JP -> do
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  LXI_SP -> do
    setStackPointer HiLo{hi,lo}
    return Next
  LXI_DE -> do
    SetReg RegD hi
    SetReg RegE lo
    return Next
  CALL -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)

pushStack :: Byte p -> Eff p ()
pushStack b = do
  sp0 <- getStackPointer >>= MakeAddr
  sp1 <- OffsetAddr (-1) sp0
  SplitAddr sp1 >>= setStackPointer
  WriteMem sp1 b

setStackPointer :: HiLo (Byte p) -> Eff p ()
setStackPointer HiLo{hi,lo} = do
  SetReg SPL lo
  SetReg SPH hi

getStackPointer :: Eff p (HiLo (Byte p))
getStackPointer = do
  lo <- GetReg SPL
  hi <- GetReg SPH
  return HiLo{hi,lo}
