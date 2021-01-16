
-- | This defines the execution semantics (Effects) of the 8080 instructions

module Semantics
  ( fetchDecodeExec
  , decodeExec
  , execInstruction
  ) where

import Prelude hiding (subtract)

import Cpu (Reg16,Reg)
import Effect (Eff(..))
import HiLo (HiLo(..))
import InstructionSet (Op(..),Instruction(..),Op0(..),Op1(..),Op2(..),RegPairSpec(..),Condition(..),cycles,justOp)
import Phase (Addr,Byte,Bit)
import qualified Cpu
import qualified Ports (inputPort,outputPort)
import InstructionSet (RegSpec(..))

-- | Semantics are defined to be Phase generic

fetchDecodeExec :: Eff p ()
fetchDecodeExec = do
  byte <- fetch
  decodeExec byte

decodeExec :: Byte p -> Eff p ()
decodeExec byte = do
  op <- Decode byte
  instruction <- fetchImmediates op
  execInstruction instruction

execInstruction :: Instruction (Byte p) -> Eff p ()
execInstruction instruction = do
  TraceInstruction instruction
  n <- execute instruction >>= \case
    Next -> do
      return $ cycles False (justOp instruction)
    Jump a -> do
      setPC a
      return $ cycles True (justOp instruction)
  Advance n

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
    a <- load16 rp
    b <- load A
    WriteMem a b
    return Next
  INX rp -> do
    a <- load16 rp
    a' <- OffsetAddr 1 a
    save16 rp a'
    return Next
  INR reg -> do
    v0 <- load reg
    cin <- MakeBit True
    zero <- MakeByte 0
    (v,_coutIgnored) <- AddWithCarry cin v0 zero
    aux <- addForAuxCarry cin v0 zero
    SetFlag Cpu.FlagA aux
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
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 7
    shifted <- byte `ShiftLeft` one
    rotated <- UpdateBit shifted 0 shunted
    SetFlag Cpu.FlagCY shunted
    save A rotated
    return Next
  RAL -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 7
    shifted <- byte `ShiftLeft` one
    cin <- GetFlag Cpu.FlagCY
    rotated <- UpdateBit shifted 0 cin
    SetFlag Cpu.FlagCY shunted
    save A rotated
    return Next
  DAA -> do
    byteIn <- load A
    auxIn <- GetFlag Cpu.FlagA
    cin <- GetFlag Cpu.FlagCY
    (byteOut,auxOut,cout) <- decimalAdjust auxIn cin byteIn
    SetFlag Cpu.FlagA auxOut
    SetFlag Cpu.FlagCY cout
    save A byteOut
    setFlagsFrom byteOut
    return Next
  STC -> do
    bit <- MakeBit True
    SetFlag Cpu.FlagCY bit
    return Next
  DAD rp -> do
    w1 <- load16 rp
    w2 <- load16 HL
    (w,cout) <- Add16 w1 w2
    save16 HL w
    SetFlag Cpu.FlagCY cout
    return Next
  LDAX rp -> do
    a <- load16 rp
    b <- ReadMem a
    save A b
    return Next
  DCX rp -> do
    a <- load16 rp
    a' <- OffsetAddr (-1) a
    save16 rp a'
    return Next
  RRC -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 0
    shifted <- byte `ShiftRight` one
    rotated <- UpdateBit shifted 7 shunted
    SetFlag Cpu.FlagCY shunted
    save A rotated
    return Next
  RAR -> do
    byte <- load A
    one <- MakeByte 1
    shunted <- byte `TestBit` 0
    shifted <- byte `ShiftRight` one
    cin <- GetFlag Cpu.FlagCY
    rotated <- UpdateBit shifted 7 cin
    SetFlag Cpu.FlagCY shunted
    save A rotated
    return Next
  CMA -> do
    v0 <- load A
    v <- Complement v0
    save A v
    return Next
  CMC -> do -- untested
    bit <- GetFlag Cpu.FlagCY
    bit' <- Flip bit
    SetFlag Cpu.FlagCY bit'
    return Next
  MOV {dest,src} -> do
    b <- load src
    save dest b
    return Next
  HLT -> do
    --Unimplemented "HLT"
    pc <- getPC -- already advanced
    pcBack <- OffsetAddr (-1) pc
    return $ Jump pcBack
  ADD reg -> do
    v1 <- load reg
    cin <- MakeBit False
    addToAccWithCarry cin v1
  ADC reg -> do
    v1 <- load reg
    cin <- GetFlag Cpu.FlagCY
    addToAccWithCarry cin v1
  SUB reg -> do
    v1 <- load reg
    cin <- MakeBit False
    subToAccWithCarry cin v1
  SBB reg -> do
    v1 <- load reg
    cin <- GetFlag Cpu.FlagCY
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
    v1 <- load A
    v2 <- load reg
    (v,borrow) <- subtract v1 v2
    setFlagsFrom v
    SetFlag Cpu.FlagCY borrow
    return Next
  RCond cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> do
        lo <- popStack
        hi <- popStack
        dest <- MakeAddr $ HiLo{hi,lo}
        return (Jump dest)
  POP rp -> do
    lo <- popStack
    hi <- popStack
    case expandRegPair rp of
      Right rr -> do
        a <- MakeAddr $ HiLo{hi,lo}
        SetReg16 rr a
      Left (HiLo{hi=rh, lo=rl}) -> do
        setRegOrFlags rh hi
        setRegOrFlags rl lo
    return Next
  XTHL -> do
    sp0 <- load16 SP
    sp1 <- OffsetAddr 1 sp0
    stack0 <- ReadMem sp0
    stack1 <- ReadMem sp1
    l <- load L
    h <- load H
    WriteMem sp0 l
    WriteMem sp1 h
    save L stack0
    save H stack1
    return Next
  DI -> do
    DisableInterrupts
    return Next
  PUSH rp -> do
    HiLo{hi,lo} <-
      case expandRegPair rp of
        Right rr -> do
          a <- GetReg16 rr
          SplitAddr a
        Left (HiLo{hi=rh, lo=rl}) -> do
          hi <- getRegOrFlags rh
          lo <- getRegOrFlags rl
          pure $ HiLo{hi,lo}
    pushStack hi
    pushStack lo
    return Next
  RST w -> do
    GetReg Cpu.PCH >>= pushStack
    GetReg Cpu.PCL >>= pushStack
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
    dest <- load16 HL
    return (Jump dest)
  SPHL -> do
    word <- load16 HL
    save16 SP word
    return Next
  XCHG -> do  -- maybe use load16/save16? (if DE becomes a reg16)
    d <- load D
    e <- load E
    h <- load H
    l <- load L
    save D h
    save E l
    save H d
    save L e
    return Next
  EI -> do
    EnableInterrupts
    return Next


decimalAdjust :: Bit p -> Bit p -> Byte p-> Eff p (Byte p, Bit p, Bit p)
decimalAdjust auxIn cin byteIn = do

  mask <- MakeByte 0xF
  lo <- AndB byteIn mask
  loMoreThan9 <- nibbleAbove9 lo
  loNeedsAdjust <- OrBit loMoreThan9 auxIn
  loPlus6 <- add6 lo
  loAdjust <- Ite loNeedsAdjust loPlus6 lo
  auxOut <- TestBit loAdjust 4

  four <- MakeByte 0x4
  hi0 <- byteIn `ShiftRight` four
  hi <- incrementMaybe auxOut hi0

  hiMoreThan9 <- nibbleAbove9 hi
  hiNeedsAdjust <- OrBit hiMoreThan9 cin
  hiPlus6 <- add6 hi
  hiAdjust <- Ite hiNeedsAdjust hiPlus6 hi

  cout0 <- TestBit hiAdjust 4
  cout <- OrBit cout0 cin

  lo' <- AndB loAdjust mask
  hi' <- hiAdjust `ShiftLeft` four
  byteOut <- OrB hi' lo'

  return (byteOut,auxOut,cout)


nibbleAbove9 :: Byte p -> Eff p (Bit p)
nibbleAbove9 n = do
  n1 <- n `TestBit` 1 -- 2
  n2 <- n `TestBit` 2 -- 4
  n3 <- n `TestBit` 3 -- 8
  n12 <- OrBit n1 n2
  AndBit n12 n3

incrementMaybe :: Bit p -> Byte p -> Eff p (Byte p)
incrementMaybe cin v = do
  zero <- MakeByte 0x0
  (v',_) <- AddWithCarry cin v zero
  return v'

add6 :: Byte p -> Eff p (Byte p)
add6 v = do
  false <- MakeBit False
  six <- MakeByte 0x6
  (vPlus6,_) <- AddWithCarry false v six
  return vPlus6


executeCond :: Condition -> Eff p (Bit p)
executeCond = \case
  NZ -> GetFlag Cpu.FlagZ >>= Flip
  Z -> GetFlag Cpu.FlagZ
  NC -> GetFlag Cpu.FlagCY >>= Flip
  CY -> GetFlag Cpu.FlagCY
  PO -> GetFlag Cpu.FlagP >>= Flip
  PE -> GetFlag Cpu.FlagP
  P -> GetFlag Cpu.FlagS >>= Flip
  MI -> GetFlag Cpu.FlagS


load :: RegSpec -> Eff p (Byte p)
load = \case
  A -> GetReg Cpu.A
  B -> GetReg Cpu.B
  C -> GetReg Cpu.C
  D -> GetReg Cpu.D
  E -> GetReg Cpu.E
  H -> load16hi HL
  L -> load16lo HL
  M -> load16 HL >>= ReadMem

load16hi :: RegPairSpec -> Eff p (Byte p)
load16hi rr = do
  a <- load16 rr
  HiLo{hi} <- SplitAddr a
  pure hi

load16lo :: RegPairSpec -> Eff p (Byte p)
load16lo rr = do
  a <- load16 rr
  HiLo{lo} <- SplitAddr a
  pure lo

save :: RegSpec -> Byte p -> Eff p ()
save = \case
  A -> SetReg Cpu.A
  B -> SetReg Cpu.B
  C -> SetReg Cpu.C
  D -> SetReg Cpu.D
  E -> SetReg Cpu.E
  H -> save16hi HL
  L -> save16lo HL
  M -> \b -> do a <- load16 HL; WriteMem a b

execute1 :: Op1 -> Byte p -> Eff p (Flow p)
execute1 op1 b1 = case op1 of
  MVI dest -> do
    save dest b1
    return Next
  OUT -> do
    value <- load A
    Ports.outputPort b1 value
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
    value <- Ports.inputPort b1
    save A value
    return Next
  ACI -> do
    cin <- GetFlag Cpu.FlagCY
    addToAccWithCarry cin b1
  SBI -> do
    cin <- GetFlag Cpu.FlagCY
    subToAccWithCarry cin b1
  XRI -> do
    binop XorB b1
  CPI -> do
    b <- load A
    (v,borrow) <- subtract b b1
    setFlagsFrom v
    SetFlag Cpu.FlagCY borrow
    return Next


binop
  :: (Byte p -> Byte p -> Eff p (Byte p))
  -> Byte p
  -> Eff p (Flow p)
binop f b1 = do
  v0 <- load A
  v <- f b1 v0
  saveAndSetFlagsFrom A v
  resetCarry
  resetAux
  return Next

andA
  :: Byte p
  -> Eff p (Flow p)
andA b1 = do
  v0 <- load A
  v <- AndB b1 v0
  saveAndSetFlagsFrom A v
  resetCarry
  w <- OrB b1 v0
  aux <- TestBit w 3
  SetFlag Cpu.FlagA aux
  return Next

resetCarry :: Eff p ()
resetCarry = do
  c <- MakeBit False
  SetFlag Cpu.FlagCY c

resetAux :: Eff p ()
resetAux = do
  a <- MakeBit False
  SetFlag Cpu.FlagA a


execute2 :: Op2 -> Addr p -> Eff p (Flow p)
execute2 op2 a = case op2 of
  LXI rp -> do
    save16 rp a
    return Next
  SHLD -> do
    b <- load L
    WriteMem a b
    a' <- OffsetAddr 1 a
    b' <- load H
    WriteMem a' b'
    return Next
  STA -> do
    b  <- load A
    WriteMem a b
    return Next
  LHLD -> do
    b <- ReadMem a
    save L b
    a' <- OffsetAddr 1 a
    b' <- ReadMem a'
    save H b'
    return Next
  LDA -> do
    b <- ReadMem a
    save A b
    return Next
  JCond cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> return (Jump a)
  JMP -> do
    return (Jump a)
  JMPx -> do
    return (Jump a)
  CCond cond -> do
    executeCond cond >>= CaseBit >>= \case
      False -> return Next
      True -> call a
  CALL ->
    call a
  CALLx{} ->
    call a


addToAccWithCarry :: Bit p -> Byte p -> Eff p (Flow p)
addToAccWithCarry cin v1 = do
  v2 <- load A
  (v,cout) <- AddWithCarry cin v1 v2
  aux <- addForAuxCarry cin v1 v2
  SetFlag Cpu.FlagA aux
  SetFlag Cpu.FlagCY cout
  saveAndSetFlagsFrom A v
  return Next


subToAccWithCarry :: Bit p -> Byte p -> Eff p (Flow p)
subToAccWithCarry cin v2 = do
  v1 <- load A
  (v,cout) <- subWithCarry cin v1 v2
  SetFlag Cpu.FlagCY cout
  saveAndSetFlagsFrom A v
  return Next


subtract :: Byte p -> Byte p -> Eff p (Byte p, Bit p)
subtract v1 v2 = do
  cin <- MakeBit False
  subWithCarry cin v1 v2

subWithCarry :: Bit p -> Byte p -> Byte p -> Eff p (Byte p, Bit p)
subWithCarry cin v1 v2 = do
  cin' <- Flip cin
  v2comp <- Complement v2
  (v,cout) <- AddWithCarry cin' v1 v2comp
  aux <- addForAuxCarry cin' v1 v2comp
  SetFlag Cpu.FlagA aux
  borrow <- Flip cout
  return (v, borrow)


addForAuxCarry :: Bit p -> Byte p -> Byte p -> Eff p (Bit p)
addForAuxCarry cin v1 v2 = do
  mask <- MakeByte 0xF
  v1masked <- AndB v1 mask
  v2masked <- AndB v2 mask
  (nibbleSum,_coutIgnored) <- AddWithCarry cin v1masked v2masked
  TestBit nibbleSum 4


call :: Addr p -> Eff p (Flow p)
call a = do
  hi <- GetReg Cpu.PCH
  lo <- GetReg Cpu.PCL
  pushStack hi
  pushStack lo
  returnAddr <- MakeAddr $ HiLo{hi,lo}
  MarkReturnAddress returnAddr -- for reachabiity
  return (Jump a)


saveAndSetFlagsFrom :: RegSpec -> Byte p -> Eff p ()
saveAndSetFlagsFrom reg v = do
  save reg v
  setFlagsFrom v

setFlagsFrom :: Byte p -> Eff p ()
setFlagsFrom value = do
  s <- IsSigned value
  z <- IsZero value
  p <- IsParity value
  SetFlag Cpu.FlagS s
  SetFlag Cpu.FlagZ z
  SetFlag Cpu.FlagP p


pushStack :: Byte p -> Eff p ()
pushStack b = do
  sp0 <- load16 SP
  sp1 <- OffsetAddr (-1) sp0
  save16 SP sp1
  WriteMem sp1 b

popStack :: Eff p (Byte p)
popStack = do
  sp0 <- load16 SP
  sp1 <- OffsetAddr 1 sp0
  save16 SP sp1
  ReadMem sp0

getPC :: Eff p (Addr p)
getPC = do
  hi <- GetReg Cpu.PCH
  lo <- GetReg Cpu.PCL
  MakeAddr $ HiLo{hi,lo}

setPC :: Addr p -> Eff p ()
setPC a = do
  HiLo{hi,lo} <- SplitAddr a
  SetReg Cpu.PCL lo
  SetReg Cpu.PCH hi

load16 :: RegPairSpec -> Eff p (Addr p)
load16 rp = do
  case expandRegPair rp of
    Right rr -> GetReg16 rr
    Left (HiLo{hi=rh, lo=rl}) -> do
      hi <- GetReg rh
      lo <- GetReg rl
      MakeAddr $ HiLo{hi,lo}

save16 :: RegPairSpec -> Addr p -> Eff p ()
save16 rp a = do
  case expandRegPair rp of
    Right rr -> SetReg16 rr a
    Left (HiLo{hi=rh, lo=rl}) -> do
      HiLo{hi,lo} <- SplitAddr a
      SetReg rh hi
      SetReg rl lo

save16hi :: RegPairSpec -> Byte p -> Eff p ()
save16hi rp hi = do
  case expandRegPair rp of
    Right rr -> do
      a <- GetReg16 rr
      HiLo{lo} <- SplitAddr a
      a' <- MakeAddr $ HiLo{hi,lo}
      SetReg16 rr a'
    Left (HiLo{hi=rh}) -> do
      SetReg rh hi

save16lo :: RegPairSpec -> Byte p -> Eff p ()
save16lo rp lo = do
  case expandRegPair rp of
    Right rr -> do
      a <- GetReg16 rr
      HiLo{hi} <- SplitAddr a
      a' <- MakeAddr $ HiLo{hi,lo}
      SetReg16 rr a'
    Left (HiLo{lo=rl}) -> do
      SetReg rl lo


expandRegPair :: RegPairSpec -> Either (HiLo Reg) Reg16
expandRegPair = \case
  BC -> Left $ HiLo {hi = Cpu.B, lo = Cpu.C}
  DE -> Left $ HiLo {hi = Cpu.D, lo = Cpu.E}
  HL -> Right Cpu.HL
  SP -> Right Cpu.SP
  PSW -> Left $ HiLo {hi = Cpu.A, lo = Cpu.Flags}

setRegOrFlags :: Reg -> Byte p -> Eff p ()
setRegOrFlags r v = case r of
  Cpu.Flags -> do
    TestBit v 7 >>= SetFlag Cpu.FlagS
    TestBit v 6 >>= SetFlag Cpu.FlagZ
    TestBit v 4 >>= SetFlag Cpu.FlagA
    TestBit v 2 >>= SetFlag Cpu.FlagP
    TestBit v 0 >>= SetFlag Cpu.FlagCY
  reg ->
    SetReg reg v

getRegOrFlags :: Reg -> Eff p (Byte p)
getRegOrFlags = \case
  Cpu.Flags -> do
    x <- MakeByte 0x2
    x <- GetFlag Cpu.FlagS >>= UpdateBit x 7
    x <- GetFlag Cpu.FlagZ >>= UpdateBit x 6
    x <- GetFlag Cpu.FlagA >>= UpdateBit x 4
    x <- GetFlag Cpu.FlagP >>= UpdateBit x 2
    x <- GetFlag Cpu.FlagCY >>= UpdateBit x 0
    return x
  reg ->
    GetReg reg
