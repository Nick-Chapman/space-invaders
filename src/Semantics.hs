
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Semantics (fetchDecodeExec) where

import Prelude hiding (subtract)

import Cpu (Flag(..),Reg(..))
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op(..),Instruction(..),Op0(..),Op1(..),Op2(..),RegPairSpec(..),Condition(..),cycles)
import Phase (Addr,Byte,Bit)
import qualified Ports (inputPort,outputPort)
import qualified InstructionSet as Instr (RegSpec(..))

-- | Semantics are defined to be Phase generic

fetchDecodeExec :: Eff p (Instruction (Byte p), Int)
fetchDecodeExec = do
    byte <- fetchOrHandleInterrupt
    op <- Decode byte
    instruction <- fetchImmediates op
    execute instruction >>= \case
      Next -> do
        let n = cycles False op
        return (instruction,n)
      Jump a -> do
        let n = cycles True op
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
  NOPx{} -> do
    return Next
  RET -> do
    lo <- popStack
    hi <- popStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  RETx{} -> do
    lo <- popStack
    hi <- popStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  RCond cond -> do
    executeCond cond >>= TestBit >>= \case
      False -> return Next
      True -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump dest)
  RRC -> do
    byte <- GetReg A
    (byte',bool') <- RotateRight byte
    SetFlag FlagCY bool'
    SetReg A byte'
    return Next
  RAL -> do
    byte <- GetReg A
    bool <- GetFlag FlagCY
    (byte',bool') <- RotateLeftThroughCarry (bool,byte)
    SetFlag FlagCY bool'
    SetReg A byte'
    return Next
  RLC -> do
    byte <- GetReg A
    (byte',bool') <- RotateLeft byte
    SetFlag FlagCY bool'
    SetReg A byte'
    return Next
  RAR -> do
    byte <- GetReg A
    bool <- GetFlag FlagCY
    (byte',bool') <- RotateRightThroughCarry (bool,byte)
    SetFlag FlagCY bool'
    SetReg A byte'
    return Next
  EI -> do
    EnableInterrupts
    return Next
  DAA -> do
    -- TODO: ignored
    return Next
  STC -> do
    bit <- MakeBit True
    SetFlag FlagCY bit
    return Next
  XCHG -> do
    d <- GetReg D
    e <- GetReg E
    h <- GetReg H
    l <- GetReg L
    SetReg D h
    SetReg E l
    SetReg H d
    SetReg L e
    return Next
  XTHL -> do
    sp0 <- getRegPair SP
    sp1 <- OffsetAddr 1 sp0
    stack0 <- ReadMem sp0
    stack1 <- ReadMem sp1
    l <- GetReg L
    h <- GetReg H
    WriteMem sp0 l
    WriteMem sp1 h
    SetReg L stack0
    SetReg H stack1
    return Next
  PCHL -> do
    dest <- getRegPair HL
    return (Jump dest)
  CMA -> do
    v0 <- GetReg A
    v <- Complement v0
    SetReg A v
    return Next
  LDAX rp -> do
    a <- getRegPair rp
    b <- ReadMem a
    SetReg A b
    return Next
  MOV {dest,src} -> do
    b <- load src
    save dest b
    return Next
  INX rp -> do
    a <- getRegPair rp
    a' <- OffsetAddr 1 a
    setRegPair rp a'
    return Next
  DCX rp -> do
    a <- getRegPair rp
    a' <- OffsetAddr (-1) a
    setRegPair rp a'
    return Next
  PUSH rp -> do
    let HiLo{hi=rh, lo=rl} = expandRegPairX rp
    hi <- getRegX rh
    lo <- getRegX rl
    pushStack hi
    pushStack lo
    return Next
  POP rp -> do
    lo <- popStack
    hi <- popStack
    let HiLo{hi=rh, lo=rl} = expandRegPairX rp
    setRegX rh hi
    setRegX rl lo
    return Next
  DAD rp -> do
    w1 <- getRegPair rp
    w2 <- getRegPair HL
    (w,cout) <- Add16 w1 w2
    setRegPair HL w
    SetFlag FlagCY cout
    return Next
  INR reg -> do
    v0 <- load reg
    v <- Increment v0
    save reg v
    setFlagsFrom v
    return Next
  DCR reg -> do
    v0 <- load reg
    v <- Decrement v0
    save reg v
    setFlagsFrom v
    return Next
  ADD reg -> do
    v1 <- load reg
    v2 <- GetReg A
    cin <- MakeBit False
    (v,cout) <- AddWithCarry cin v1 v2
    SetReg A v
    SetFlag FlagCY cout
    setFlagsFrom v
    return Next
  ADC reg -> do
    v1 <- load reg
    v2 <- GetReg A
    cin <- GetFlag FlagCY
    (v,cout) <- AddWithCarry cin v1 v2
    SetReg A v
    SetFlag FlagCY cout
    setFlagsFrom v
    return Next
  SUB reg -> do
    v1 <- load reg
    v2 <- GetReg A
    cin <- MakeBit False
    (v,cout) <- subWithCarry cin v1 v2
    SetReg A v
    SetFlag FlagCY cout
    setFlagsFrom v
    return Next
  SBB reg -> do
    v1 <- load reg
    v2 <- GetReg A
    cin <- GetFlag FlagCY
    (v,cout) <- subWithCarry cin v1 v2
    SetReg A v
    SetFlag FlagCY cout
    setFlagsFrom v
    return Next
  XRA reg -> do
    v1 <- load reg
    v2 <- GetReg A
    v <- XorB v1 v2
    SetReg A v
    setFlagsFrom v
    resetCarry
    return Next
  ANA reg -> do
    v1 <- load reg
    v2 <- GetReg A
    v <- AndB v1 v2
    SetReg A v
    setFlagsFrom v
    resetCarry
    return Next
  ORA reg -> do
    v1 <- load reg
    v2 <- GetReg A
    v <- OrB v1 v2
    SetReg A v
    setFlagsFrom v
    resetCarry
    return Next
  CMP reg -> do
    v1 <- GetReg A
    v2 <- load reg
    (v,borrow) <- subtract v1 v2
    setFlagsFrom v
    SetFlag FlagCY borrow
    return Next
  RST w -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    hi <- MakeByte 0
    lo <- MakeByte (8*w)
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)


resetCarry :: Eff p ()
resetCarry = do
  c <- MakeBit False
  SetFlag FlagCY c


executeCond :: Condition -> Eff p (Bit p)
executeCond = \case
  NZ -> GetFlag FlagZ >>= Flip
  Z -> GetFlag FlagZ
  NC -> GetFlag FlagCY >>= Flip
  CY -> GetFlag FlagCY
  PO -> error "executeCond, PO, need parity flag"
  PE -> error "executeCond, PE, need parity flag"
  P -> GetFlag FlagS >>= Flip
  MI -> GetFlag FlagS

load :: Instr.RegSpec -> Eff p (Byte p)
load = \case
  Instr.A -> GetReg A
  Instr.B -> GetReg B
  Instr.C -> GetReg C
  Instr.D -> GetReg D
  Instr.E -> GetReg E
  Instr.H -> GetReg H
  Instr.L -> GetReg L
  Instr.M -> getRegPair HL >>= ReadMem


save :: Instr.RegSpec -> Byte p -> Eff p ()
save = \case
  Instr.A -> SetReg A
  Instr.B -> SetReg B
  Instr.C -> SetReg C
  Instr.D -> SetReg D
  Instr.E -> SetReg E
  Instr.H -> SetReg H
  Instr.L -> SetReg L
  Instr.M -> \b -> do a <- getRegPair HL; WriteMem a b


execute1 :: Op1 -> Byte p -> Eff p (Flow p)
execute1 op1 b1 = case op1 of
  MVI dest -> do
    save dest b1
    return Next
  CPI -> do
    b <- GetReg A
    (v,borrow) <- subtract b b1
    setFlagsFrom v
    SetFlag FlagCY borrow
    return Next
  OUT -> do
    port <- DispatchByte b1
    value <- GetReg A
    Ports.outputPort port value
    return Next
  IN -> do
    port <- DispatchByte b1
    value <- Ports.inputPort port
    SetReg A value
    return Next
  ANI -> do
    v0 <- GetReg A
    v <- AndB b1 v0
    SetReg A v
    setFlagsFrom v
    resetCarry
    return Next
  ORI -> do
    v0 <- GetReg A
    v <- OrB b1 v0
    SetReg A v
    setFlagsFrom v
    resetCarry
    return Next
  ADI -> do
    v0 <- GetReg A
    cin <- MakeBit False
    (v,cout) <- AddWithCarry cin v0 b1
    SetFlag FlagCY cout
    SetReg A v
    setFlagsFrom v
    return Next
  SUI -> do
    v0 <- GetReg A
    cin <- MakeBit True
    b1comp <- Complement b1
    (v,cout) <- AddWithCarry cin v0 b1comp
    cout' <- Flip cout
    SetFlag FlagCY cout'
    SetReg A v
    setFlagsFrom v
    return Next
  ACI -> do
    Unimplemented "ACI"
  SBI -> do -- TODO: share code with SUI...
    v0 <- GetReg A
    cin <- GetFlag FlagCY -- only change here
    cin' <- Flip cin -- and here (is flip correct?)
    b1comp <- Complement b1
    (v,cout) <- AddWithCarry cin' v0 b1comp
    cout' <- Flip cout
    SetFlag FlagCY cout'
    SetReg A v
    setFlagsFrom v
    return Next
  XRI -> do
    Unimplemented "XRI"


setFlagsFrom :: Byte p -> Eff p ()
setFlagsFrom value = do
  s <- IsSigned value
  z <- IsZero value
  SetFlag FlagS s
  SetFlag FlagZ z

execute2 :: Op2 -> (Byte p, Byte p) -> Eff p (Flow p)
execute2 op2 (lo,hi) = case op2 of
  JMP -> do
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  JMPx -> do
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  JCond cond -> do
    executeCond cond >>= TestBit >>= \case
      False -> return Next
      True -> do
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
  CALLx{} -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
  CCond cond -> do
    executeCond cond >>= TestBit >>= \case
      False -> return Next
      True -> do
        GetReg PCH >>= pushStack
        GetReg PCL >>= pushStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump dest)
  LDA -> do
    a <- MakeAddr $ HiLo{hi,lo}
    b <- ReadMem a
    SetReg A b
    return Next
  STA -> do
    a <- MakeAddr $ HiLo{hi,lo}
    b  <- GetReg A
    WriteMem a b
    return Next
  LHLD -> do
    a <- MakeAddr $ HiLo{hi,lo}
    b <- ReadMem a
    SetReg L b
    a' <- OffsetAddr 1 a
    b' <- ReadMem a'
    SetReg H b'
    return Next
  SHLD -> do
    a <- MakeAddr $ HiLo{hi,lo}
    b <- GetReg L
    WriteMem a b
    a' <- OffsetAddr 1 a
    b' <- GetReg H
    WriteMem a' b'
    return Next


subtract :: Byte p -> Byte p -> Eff p (Byte p, Bit p)
subtract v1 v2 = do
  cin <- MakeBit True
  subWithCarry cin v1 v2

subWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
subWithCarry cin v1 v2 = do
  v2comp <- Complement v2
  (v,cout) <- AddWithCarry cin v1 v2comp
  borrow <- Flip cout
  return (v, borrow)


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

getRegPair :: RegPairSpec -> Eff p (Addr p)
getRegPair rp = do
  let HiLo{hi=rh, lo=rl} = expandRegPair rp
  hi <- GetReg rh
  lo <- GetReg rl
  MakeAddr $ HiLo{hi,lo}

setRegPair :: RegPairSpec -> Addr p -> Eff p ()
setRegPair rp a = do
  let HiLo{hi=rh, lo=rl} = expandRegPair rp
  HiLo{hi,lo} <- SplitAddr a
  SetReg rh hi
  SetReg rl lo

expandRegPair :: RegPairSpec -> HiLo Reg
expandRegPair = \case
  BC -> HiLo {hi = B, lo = C}
  DE -> HiLo {hi = D, lo = E}
  HL -> HiLo {hi = H, lo = L}
  SP -> HiLo {hi = SPH, lo = SPL}
  PSW -> undefined

data RegX = NormalReg Reg | FlagsReg

expandRegPairX :: RegPairSpec -> HiLo RegX -- for push/pop
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
    (s,z,cy) <- SelectSZC v
    SetFlag FlagS s
    SetFlag FlagZ z
    SetFlag FlagCY cy

getRegX :: RegX -> Eff p (Byte p)
getRegX r = case r of
  NormalReg reg -> GetReg reg
  FlagsReg -> do
    s <- GetFlag FlagS
    z <- GetFlag FlagZ
    cy <- GetFlag FlagCY
    ByteFromSZC (s,z,cy)
