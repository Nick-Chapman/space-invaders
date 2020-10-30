
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
  Ins2 op2 lo hi -> do
    a <- MakeAddr $ HiLo{hi,lo}
    execute2 op2 a

execute0 :: Op0 -> Eff p (Flow p)
execute0 = \case
  NOP -> do
    return Next
  NOPx{} -> do
    return Next
  STAX rp -> do
    a <- getRegPair rp
    b <- GetReg A
    WriteMem a b
    return Next
  INX rp -> do
    a <- getRegPair rp
    a' <- OffsetAddr 1 a
    setRegPair rp a'
    return Next
  INR reg -> do
    v0 <- load reg
    cin <- MakeBit True
    zero <- MakeByte 0
    (v,aux,_) <- AddWithCarry cin v0 zero
    SetFlag FlagA aux
    saveAndSetFlagsFrom reg v
    return Next
  DCR reg -> do
    v0 <- load reg
    cin <- MakeBit True
    zero <- MakeByte 0
    (v,_) <- subWithCarry cin v0 zero
    saveAndSetFlagsFrom reg v
    return Next
  RLC -> do
    byte <- GetReg A
    (byte',bool') <- RotateLeft byte
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
  DAA -> do
    byteIn <- GetReg A
    auxIn <- GetFlag FlagA
    cin <- GetFlag FlagCY
    (byteOut,auxOut,cout) <- DecimalAdjust auxIn cin byteIn
    SetFlag FlagA auxOut
    SetFlag FlagCY cout
    SetReg A byteOut
    setFlagsFrom byteOut
    return Next
  STC -> do
    bit <- MakeBit True
    SetFlag FlagCY bit
    return Next
  DAD rp -> do
    w1 <- getRegPair rp
    w2 <- getRegPair HL
    (w,cout) <- Add16 w1 w2
    setRegPair HL w
    SetFlag FlagCY cout
    return Next
  LDAX rp -> do
    a <- getRegPair rp
    b <- ReadMem a
    SetReg A b
    return Next
  DCX rp -> do
    a <- getRegPair rp
    a' <- OffsetAddr (-1) a
    setRegPair rp a'
    return Next
  RRC -> do
    byte <- GetReg A
    (byte',bool') <- RotateRight byte
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
  CMA -> do
    v0 <- GetReg A
    v <- Complement v0
    SetReg A v
    return Next
  CMC -> do -- untested
    bit <- GetFlag FlagCY
    bit' <- Flip bit
    SetFlag FlagCY bit'
    return Next
  MOV {dest,src} -> do
    b <- load src
    save dest b
    return Next
  HLT -> do
    Unimplemented "HLT"
  ADD reg -> do
    v1 <- load reg
    cin <- MakeBit False
    addToAccWithCarry cin v1
  ADC reg -> do
    v1 <- load reg
    cin <- GetFlag FlagCY
    addToAccWithCarry cin v1
  SUB reg -> do
    v1 <- load reg
    cin <- MakeBit False
    subToAccWithCarry cin v1
  SBB reg -> do
    v1 <- load reg
    cin <- GetFlag FlagCY
    subToAccWithCarry cin v1
  ANA reg -> do
    v1 <- load reg
    andA v1
  XRA reg -> do
    v1 <- load reg
    binop XorB v1
  ORA reg -> do
    v1 <- load reg
    binop OrB v1
  CMP reg -> do
    v1 <- GetReg A
    v2 <- load reg
    (v,borrow) <- subtract v1 v2
    setFlagsFrom v
    SetFlag FlagCY borrow
    return Next
  RCond cond -> do
    executeCond cond >>= TestBit >>= \case
      False -> return Next
      True -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump dest)
  POP rp -> do
    lo <- popStack
    hi <- popStack
    let HiLo{hi=rh, lo=rl} = expandRegPair rp
    setRegOrFlags rh hi
    setRegOrFlags rl lo
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
  DI -> do
    DisableInterrupts
    return Next
  PUSH rp -> do
    let HiLo{hi=rh, lo=rl} = expandRegPair rp
    hi <- getRegOrFlags rh
    lo <- getRegOrFlags rl
    pushStack hi
    pushStack lo
    return Next
  RST w -> do
    GetReg PCH >>= pushStack
    GetReg PCL >>= pushStack
    hi <- MakeByte 0
    lo <- MakeByte (8*w)
    dest <- MakeAddr $ HiLo{hi,lo}
    return (Jump dest)
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
  PCHL -> do
    dest <- getRegPair HL
    return (Jump dest)
  SPHL -> do
    Unimplemented "SPHL"
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
  EI -> do
    EnableInterrupts
    return Next


executeCond :: Condition -> Eff p (Bit p)
executeCond = \case
  NZ -> GetFlag FlagZ >>= Flip
  Z -> GetFlag FlagZ
  NC -> GetFlag FlagCY >>= Flip
  CY -> GetFlag FlagCY
  PO -> GetFlag FlagP >>= Flip
  PE -> GetFlag FlagP
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
  OUT -> do
    port <- DispatchByte b1
    value <- GetReg A
    Ports.outputPort port value
    return Next
  ADI -> do
    cin <- MakeBit False
    addToAccWithCarry cin b1
  SUI -> do
    cin <- MakeBit False
    subToAccWithCarry cin b1
  ANI ->
    andA b1
  ORI ->
    binop OrB b1
  IN -> do
    port <- DispatchByte b1
    value <- Ports.inputPort port
    SetReg A value
    return Next
  ACI -> do
    cin <- GetFlag FlagCY
    addToAccWithCarry cin b1
  SBI -> do
    cin <- GetFlag FlagCY
    subToAccWithCarry cin b1
  XRI -> do
    binop XorB b1
  CPI -> do
    b <- GetReg A
    (v,borrow) <- subtract b b1
    setFlagsFrom v
    SetFlag FlagCY borrow
    return Next


binop
  :: (Byte p -> Byte p -> Eff p (Byte p))
  -> Byte p
  -> Eff p (Flow p)
binop f b1 = do
  v0 <- GetReg A
  v <- f b1 v0
  saveAndSetFlagsFrom Instr.A v
  resetCarry
  resetAux
  return Next

andA
  :: Byte p
  -> Eff p (Flow p)
andA b1 = do
  v0 <- GetReg A
  v <- AndB b1 v0
  saveAndSetFlagsFrom Instr.A v
  resetCarry
  w <- OrB b1 v0
  aux <- SplitByte w 3
  SetFlag FlagA aux
  return Next

resetCarry :: Eff p ()
resetCarry = do
  c <- MakeBit False
  SetFlag FlagCY c

resetAux :: Eff p ()
resetAux = do
  a <- MakeBit False
  SetFlag FlagA a


execute2 :: Op2 -> Addr p -> Eff p (Flow p)
execute2 op2 a = case op2 of
  LXI rp -> do
    setRegPair rp a
    return Next
  SHLD -> do
    b <- GetReg L
    WriteMem a b
    a' <- OffsetAddr 1 a
    b' <- GetReg H
    WriteMem a' b'
    return Next
  STA -> do
    b  <- GetReg A
    WriteMem a b
    return Next
  LHLD -> do
    b <- ReadMem a
    SetReg L b
    a' <- OffsetAddr 1 a
    b' <- ReadMem a'
    SetReg H b'
    return Next
  LDA -> do
    b <- ReadMem a
    SetReg A b
    return Next
  JCond cond -> do
    executeCond cond >>= TestBit >>= \case
      False -> return Next
      True -> return (Jump a)
  JMP -> do
    return (Jump a)
  JMPx -> do
    return (Jump a)
  CCond cond -> do
    executeCond cond >>= TestBit >>= \case
      False -> return Next
      True -> call a
  CALL ->
    call a
  CALLx{} ->
    call a


addToAccWithCarry :: Bit p -> Byte p -> Eff p (Flow p)
addToAccWithCarry cin v1 = do
  v2 <- GetReg A
  (v,aux,cout) <- AddWithCarry cin v1 v2
  SetFlag FlagA aux
  SetFlag FlagCY cout
  saveAndSetFlagsFrom Instr.A v
  return Next

subToAccWithCarry :: Bit p -> Byte p -> Eff p (Flow p)
subToAccWithCarry cin v2 = do
  v1 <- GetReg A
  (v,cout) <- subWithCarry cin v1 v2
  SetFlag FlagCY cout
  saveAndSetFlagsFrom Instr.A v
  return Next



subtract :: Byte p -> Byte p -> Eff p (Byte p, Bit p)
subtract v1 v2 = do
  cin <- MakeBit False
  subWithCarry cin v1 v2

subWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
subWithCarry cin v1 v2 = do
  cin' <- Flip cin
  v2comp <- Complement v2
  (v,aux,cout) <- AddWithCarry cin' v1 v2comp
  SetFlag FlagA aux
  borrow <- Flip cout
  return (v, borrow)




call :: Addr p -> Eff p (Flow p)
call a = do
  GetReg PCH >>= pushStack
  GetReg PCL >>= pushStack
  return (Jump a)


saveAndSetFlagsFrom :: Instr.RegSpec -> Byte p -> Eff p ()
saveAndSetFlagsFrom reg v = do
  save reg v
  setFlagsFrom v

setFlagsFrom :: Byte p -> Eff p ()
setFlagsFrom value = do
  s <- IsSigned value
  z <- IsZero value
  p <- IsParity value
  SetFlag FlagS s
  SetFlag FlagZ z
  SetFlag FlagP p


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
  PSW -> HiLo {hi = A, lo = Flags}

setRegOrFlags :: Reg -> Byte p -> Eff p ()
setRegOrFlags r v = case r of
  Flags -> do
    (s,z,a,p,cy) <- SelectSZAPC v
    SetFlag FlagS s
    SetFlag FlagZ z
    SetFlag FlagA a
    SetFlag FlagP p
    SetFlag FlagCY cy
  reg ->
    SetReg reg v

getRegOrFlags :: Reg -> Eff p (Byte p)
getRegOrFlags = \case
  Flags -> do
    s <- GetFlag FlagS
    z <- GetFlag FlagZ
    a <- GetFlag FlagA
    p <- GetFlag FlagP
    cy <- GetFlag FlagCY
    ByteFromSZAPC (s,z,a,p,cy)
  reg ->
    GetReg reg
