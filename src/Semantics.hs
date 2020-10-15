
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Semantics (fetchDecodeExec) where

import Cpu (Reg(..),RegPair(..),Flag(..))
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op(..),Instruction(..),Op0(..),Op1(..),Op2(..))
import Phase (Addr,Byte)

-- | Semantics are defined to be Phase generic

fetchDecodeExec :: Eff p ()
fetchDecodeExec = do
  InstructionCycle $ do
    byte <- fetchOrHandleInterrupt
    op <- Decode byte
    instruction <- fetchImmediates op
    execute instruction >>= \case
      Next n -> do
        return (instruction,n)
      Jump n a -> do
        setPC a
        return (instruction,n)

fetchOrHandleInterrupt :: Eff p (Byte p)
fetchOrHandleInterrupt = do
  processInterrupt >>= \case
    False -> fetch
    True -> do
      DisableInterrupts
      GetInterruptInstruction

processInterrupt :: Eff p Bool
processInterrupt = do
  isTime <- TimeToWakeup
  enabled <- AreInterruptsEnabled
  return (isTime && enabled)

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

data Flow p = Next Int | Jump Int (Addr p)

execute :: Instruction (Byte p) -> Eff p (Flow p)
execute = \case
  Ins0 op0 -> execute0 op0
  Ins1 op1 b1 -> execute1 op1 b1
  Ins2 op2 b1 b2 -> execute2 op2 (b1,b2)

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return (Next 4)
  RET -> do
    lo <- popStack
    hi <- popStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump 10 dest)
  RZ -> do
    testFlagZ >>= \case
      False -> return (Next 5)
      True -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 11 dest)
  RC -> do
    testFlagCY >>= \case
      False -> return (Next 5)
      True -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 11 dest)
  RNZ -> do
    testFlagZ >>= \case
      True -> return (Next 5)
      False -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 11 dest)
  RRC -> do
    byte <- GetReg A
    bool <- GetFlag CY
    (byte',bool') <- RotateRight (bool,byte)
    SetFlag CY bool'
    SetReg A byte'
    return (Next 4)
  EI -> do
    EnableInterrupts
    return (Next 4)
  STC -> do
    bit <- MakeBit True
    SetFlag CY bit
    return (Next 4)
  XCHG -> do
    d <- GetReg D
    e <- GetReg E
    h <- GetReg H
    l <- GetReg L
    SetReg D h
    SetReg E l
    SetReg H d
    SetReg L e
    return (Next 5)
  LDAX_B -> do
    a <- getRegPair BC
    b <- ReadMem a
    SetReg A b
    return (Next 7)
  LDAX_D -> do
    a <- getRegPair DE
    b <- ReadMem a
    SetReg A b
    return (Next 7)
  MOV_M_A -> do
    a <- getRegPair HL
    b <- GetReg A
    WriteMem a b
    return (Next 7)
  MOV_rM reg -> do
    a <- getRegPair HL
    b <- ReadMem a
    SetReg reg b
    return (Next 7)
  MOV {dest,src} -> do
    b <- GetReg src
    SetReg dest b
    return (Next 7)
  INX rp -> do
    a <- getRegPair rp
    a' <- OffsetAddr 1 a
    setRegPair rp a'
    return (Next 5)
  PUSH rp -> do
    let HiLo{hi=rh, lo=rl} = expandRegPairX rp
    hi <- getRegX rh
    lo <- getRegX rl
    pushStack hi
    pushStack lo
    return (Next 11)
  POP rp -> do
    lo <- popStack
    hi <- popStack
    let HiLo{hi=rh, lo=rl} = expandRegPairX rp
    setRegX rh hi
    setRegX rl lo
    return (Next 10)
  DAD rp -> do
    w1 <- getRegPair rp
    w2 <- getRegPair HL
    w <- Add16 w1 w2
    setRegPair HL w
    -- TODO: set carry flag
    return (Next 11)
  DCR reg -> do
    v0 <- GetReg reg
    v <- Decrement v0
    SetReg reg v
    setFlagsFrom v
    return (Next 5)
  DCR_M -> do
    a <- getRegPair HL
    v0 <- ReadMem a
    v <- Decrement v0
    WriteMem a v
    setFlagsFrom v
    return (Next 10)
  XRA reg -> do
    v1 <- GetReg reg
    v2 <- GetReg A
    v <- XorB v1 v2
    SetReg reg v -- TODO: bug, should set A
    setFlagsFrom v
    return (Next 4)
  ANA reg -> do
    v1 <- GetReg reg
    v2 <- GetReg A
    v <- AndB v1 v2
    SetReg reg v -- TODO: bug, should set A
    setFlagsFrom v
    return (Next 4)
  ORA reg -> do
    v1 <- GetReg reg
    v2 <- GetReg A
    v <- OrB v1 v2
    SetReg A v
    setFlagsFrom v
    return (Next 4)
  ORA_M -> do
    a <- getRegPair HL
    v1 <- ReadMem a
    v2 <- GetReg A
    v <- OrB v1 v2
    SetReg A v
    setFlagsFrom v
    return (Next 7)
  RST w -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    hi <- MakeByte 0
    lo <- MakeByte (8*w)
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump 4 dest)

execute1 :: Op1 -> Byte p -> Eff p (Flow p)
execute1 op1 b1 = case op1 of
  MVI dest -> do
    SetReg dest b1
    return (Next 7)
  MVI_M -> do
    a <- getRegPair HL
    WriteMem a b1
    return (Next 10)
  CPI -> do
    b <- GetReg A
    cin <- MakeBit True
    b1comp <- Complement b1
    (v,cout) <- AddWithCarry cin b b1comp
    cout' <- Flip cout
    SetFlag CY cout'
    setFlagsFrom v
    return (Next 7)
  OUT -> do
    value <- GetReg A
    Out b1 value
    return (Next 10)
  IN -> do
    value <- In b1
    SetReg A value
    return (Next 10)
  ANI -> do
    v0 <- GetReg A
    v <- AndB b1 v0
    SetReg A v
    setFlagsFrom v
    return (Next 7)
  ADI -> do
    v0 <- GetReg A
    cin <- MakeBit False
    (v,cout) <- AddWithCarry cin b1 v0
    SetFlag CY cout
    SetReg A v
    setFlagsFrom v
    return (Next 7)


setFlagsFrom :: Byte p -> Eff p ()
setFlagsFrom value = do
  z <- IsZero value
  SetFlag Z z
  -- TODO: set more flags

testFlagZ :: Eff p Bool
testFlagZ = GetFlag Z >>= TestBit

testFlagCY :: Eff p Bool
testFlagCY = GetFlag CY >>= TestBit

execute2 :: Op2 -> (Byte p, Byte p) -> Eff p (Flow p)
execute2 op2 (lo,hi) = case op2 of
  JP -> do
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump 10 dest)
  JNZ -> do
    testFlagZ >>= \case
      True -> return (Next 10)
      False -> do
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 10 dest)
  JNC -> do
    testFlagCY >>= \case
      True -> return (Next 10)
      False -> do
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 10 dest)
  JZ -> do
    testFlagZ >>= \case
      False -> return (Next 10)
      True -> do
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 10 dest)
  JC -> do
    testFlagCY >>= \case
      False -> return (Next 10)
      True -> do
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump 10 dest)
  LXI rp -> do
    let HiLo{hi=rh, lo=rl} = expandRegPair rp
    SetReg rh hi
    SetReg rl lo
    return (Next 10)
  CALL -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump 17 dest)
  LDA -> do
    a <- MakeAddr $ HiLo{hi,lo}
    b <- ReadMem a
    SetReg A b
    return (Next 13)
  STA -> do
    a <- MakeAddr $ HiLo{hi,lo}
    b  <- GetReg A
    WriteMem a b
    return (Next 13)

pushStack :: Byte p -> Eff p ()
pushStack b = do
  sp0 <- getRegPair SP
  sp1 <- OffsetAddr (-1) sp0
  setRegPair SP sp1
  WriteMem sp1 b

popStack :: Eff p (Byte p)
popStack = do
  sp0 <- getRegPair SP
  sp1 <- OffsetAddr 1 sp0
  setRegPair SP sp1
  ReadMem sp0

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

expandRegPair :: RegPair -> HiLo Reg
expandRegPair = \case
  BC -> HiLo {hi = B, lo = C}
  DE -> HiLo {hi = D, lo = E}
  HL -> HiLo {hi = H, lo = L}
  SP -> HiLo {hi = SPH, lo = SPL}
  PSW -> undefined

data RegX = NormalReg Reg | FlagsReg

expandRegPairX :: RegPair -> HiLo RegX -- for push/pop
expandRegPairX = \case
  BC -> HiLo {hi = r B, lo = r C}
  DE -> HiLo {hi = r D, lo = r E}
  HL -> HiLo {hi = r H, lo = r L}
  SP -> undefined -- HiLo {hi = r SPH, lo = r SPL}
  PSW -> HiLo {hi = r A, lo = FlagsReg}
  where r = NormalReg

setRegX :: RegX -> Byte p -> Eff p ()
setRegX r v = case r of
  NormalReg reg -> SetReg reg v
  FlagsReg -> do
    -- Also do the rest of the flags we care about -- Z
    (z,cy) <- SelectBit70 v
    SetFlag Z z
    SetFlag CY cy

getRegX :: RegX -> Eff p (Byte p)
getRegX r = case r of
  NormalReg reg -> GetReg reg
  FlagsReg -> do
    -- Also do the rest of the flags we care about -- Z
    z <- GetFlag Z
    cy <- GetFlag CY
    ByteFromBit70 (z,cy)
