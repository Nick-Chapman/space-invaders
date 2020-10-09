
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Semantics (setPC,fetchDecodeExec) where

import Cpu (Reg(..))
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op(..),Instruction(..),Op0(..),Op1(..),Op2(..),RP(..))
import Phase (Addr,Byte)

-- | Semantics are defined to be Phase generic

fetchDecodeExec :: Eff p ()
fetchDecodeExec = do
  InstructionCycle $ do
    pc <- getPC
    byte <- fetch
    op <- Decode (pc, byte) -- Pass pc to decode for improved error messages
    instruction <- fetchImmediates op
    execute instruction >>= \case
      Next -> return ()
      Jump a -> setPC a
    return instruction

fetch :: Eff p (Byte p) -- fetch byte at PC, and increment PC
fetch = do
  pc <- getPC
  byte <- ReadMem pc
  OffsetAddr 1 pc >>= setPC
  return byte

fetchImmediates :: Op -> Eff p (Instruction (Byte p))
fetchImmediates = \case
  Op0 op0 -> return (Ins0 op0)
  Op1 op1 -> do
    b1 <- fetch
    return (Ins1 op1 b1)
  Op2 op2 -> do
    b1 <- fetch
    b2 <- fetch
    return (Ins2 op2 b1 b2)

data Flow p = Next | Jump (Addr p)

execute :: Instruction (Byte p) -> Eff p (Flow p)
execute = \case
  Ins0 op0 -> execute0 op0
  Ins1 op1 b1 -> execute1 op1 b1
  Ins2 op2 b1 b2 -> execute2 op2 (b1,b2)

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return Next
  LDAX_D -> do
    a <- getDE
    b <- ReadMem a
    SetReg RegA b
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
  LXI rp -> do
    let HiLo{hi=rh, lo=rl} = evalRP rp
    SetReg rh hi
    SetReg rl lo
    return Next
  CALL -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)


-- | evalate a reg-pair descriptor to the pair of registers denoted
evalRP :: RP -> HiLo Reg
evalRP = \case
  DE -> HiLo {hi = RegD, lo = RegE}
  HL -> HiLo {hi = RegH, lo = RegL}
  SP -> HiLo {hi = SPH, lo = SPL}

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
  hi <- GetReg SPH
  lo <- GetReg SPL
  return HiLo{hi,lo}

getPC :: Eff p (Addr p)
getPC = do
  hi <- GetReg PCH
  lo <- GetReg PCL
  MakeAddr $ HiLo{hi,lo}

setPC :: Addr p -> Eff p ()
setPC a = do
  HiLo{hi,lo} <- SplitAddr a
  SetReg PCL lo
  SetReg PCH hi

getDE :: Eff p (Addr p)
getDE = do
  hi <- GetReg RegD
  lo <- GetReg RegL
  MakeAddr $ HiLo{hi,lo}
