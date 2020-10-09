
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Semantics (setPC,fetchDecodeExec) where

import Cpu (Reg(..),RegPair(..),expandRegPair)
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op(..),Instruction(..),Op0(..),Op1(..),Op2(..))
import Phase (Addr,Byte)

-- | Semantics are defined to be Phase generic

fetchDecodeExec :: Eff p ()
fetchDecodeExec = do
  InstructionCycle $ do
    pc <- getPC
    byte <- fetch
    op <- Decode (pc, byte) -- Pass pc to decode for improved error messages
    instruction <- fetchImmediates byte op
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

fetchImmediates :: Byte p -> Op -> Eff p (Instruction (Byte p))
fetchImmediates b0 = \case
  Op0 op0 -> return (Ins0 op0 b0)
  Op1 op1 -> do
    b1 <- fetch
    return (Ins1 op1 b0 b1)
  Op2 op2 -> do
    b1 <- fetch
    b2 <- fetch
    return (Ins2 op2 b0 b1 b2)

data Flow p = Next | Jump (Addr p)

execute :: Instruction (Byte p) -> Eff p (Flow p)
execute = \case
  Ins0 op0 _ -> execute0 op0
  Ins1 op1 _ b1 -> execute1 op1 b1
  Ins2 op2 _ b1 b2 -> execute2 op2 (b1,b2)

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return Next
  LDAX_D -> do
    a <- getRegPair DE
    b <- ReadMem a
    SetReg A b
    return Next
  MVI_M_A -> do
    a <- getRegPair HL
    b <- ReadMem a
    SetReg A b
    return Next
  INX rp -> do
    a <- getRegPair rp
    a' <- OffsetAddr 1 a
    setRegPair rp a'
    return Next
  DEC_B -> do
    v <- GetReg B
    v' <- Decrement v -- TODO: this is modulus; is that correct?
    SetReg B v'
    SetFlagZ v'
    -- TODO: set more flags
    return Next


execute1 :: Op1 -> Byte p -> Eff p (Flow p)
execute1 op1 b1 = case op1 of
  MVI_B -> do
    SetReg B b1
    return Next

execute2 :: Op2 -> (Byte p, Byte p) -> Eff p (Flow p)
execute2 op2 (lo,hi) = case op2 of
  JP -> do
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  JNZ -> do
    TestFlagZ >>= \case
      True -> return Next
      False -> do
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump dest)
  LXI rp -> do
    let HiLo{hi=rh, lo=rl} = expandRegPair rp
    SetReg rh hi
    SetReg rl lo
    return Next
  CALL -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)

pushStack :: Byte p -> Eff p ()
pushStack b = do
  sp0 <- getRegPair SP
  sp1 <- OffsetAddr (-1) sp0
  setRegPair SP sp1
  WriteMem sp1 b

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

getRegPair :: RegPair -> Eff p (Addr p)
getRegPair rp = do
  let HiLo{hi=rh, lo=rl} = expandRegPair rp
  hi <- GetReg rh
  lo <- GetReg rl
  MakeAddr $ HiLo{hi,lo}

setRegPair :: RegPair -> Addr p -> Eff p ()
setRegPair rp a = do
  let HiLo{hi=rh, lo=rl} = expandRegPair rp
  HiLo{hi,lo} <- SplitAddr a
  SetReg rh hi
  SetReg rl lo
