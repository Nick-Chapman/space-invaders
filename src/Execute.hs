
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Execute (execute) where

import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op(..))
import Phase (Addr)

data Flow p = Next | Jump (Addr p)

executeFlow :: Addr p -> Op -> Eff p (Flow p)
executeFlow a0 = \case
  NOP -> do
    return Next
  JP -> do
    a1 <- IncAddr a0
    a2 <- IncAddr a1
    lo <- ReadMem a1
    hi <- ReadMem a2
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  LXI_SP -> do
    error "LXI_SP"

execute :: Addr p -> Op -> Eff p (Addr p)
execute a op = do
  executeFlow a op >>= \case
    Next -> IncAddr a
    Jump addr -> return addr
