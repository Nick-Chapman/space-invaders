
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Execute (execute0,execute2,Flow(..)) where

import Cpu (Reg(..))
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op0(..),Op2(..))
import Phase (Addr,Byte)

data Flow p = Next | Jump (Addr p)

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return Next

execute2 :: Op2 -> (Byte p, Byte p) -> Eff p (Flow p)
execute2 op2 (b1,b2) = case op2 of
  JP -> do
    dest <- MakeAddr $ HiLo{hi = b2,lo = b1}
    return (Jump dest)
  LXI_SP -> do
    SetReg SPL b1
    SetReg SPH b2
    return Next
