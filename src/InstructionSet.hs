
module InstructionSet (
  Op(..),Op0(..),Op1(..),Op2(..), RegPairSpec(..), RegSpec(..), Condition(..),
  Instruction(..), justOp,
  cycles,
  decode, encode,
  prettyInstructionBytes,
  theDecodeTable
  ) where

import Byte (Byte(..))
import Data.List (sort)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Data.Word8 (Word8)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | Op-codes. Stratified by the number of following immediate bytes
data Op = Op0 Op0 | Op1 Op1 | Op2 Op2
  deriving (Eq,Ord,Show)

-- | Ops which take zero immediate bytes. Listed in encoding order
data Op0
  = NOP
  | NOPx Word8 -- (1..7)
  | STAX RegPairSpec
  | INX RegPairSpec
  | INR RegSpec
  | DCR RegSpec
  | RLC
  | RAL
  | DAA
  | STC
  | DAD RegPairSpec
  | LDAX RegPairSpec
  | DCX RegPairSpec
  | RRC
  | RAR
  | CMA
  | CMC
  | MOV { dest :: RegSpec, src :: RegSpec }
  | HLT
  | ADD RegSpec
  | ADC RegSpec
  | SUB RegSpec
  | SBB RegSpec
  | ANA RegSpec
  | XRA RegSpec
  | ORA RegSpec
  | CMP RegSpec
  | RCond Condition
  | POP RegPairSpec
  | XTHL
  | DI
  | PUSH RegPairSpec
  | RST Word8 --(0..7)
  | RET
  | RETx
  | PCHL
  | SPHL
  | XCHG
  | EI
  deriving (Eq,Ord,Show)

-- | Ops which take one immediate byte.
data Op1
  = MVI RegSpec
  | OUT
  | ADI
  | SUI -- subtract
  | ANI
  | ORI
  | IN
  | ACI
  | SBI -- subtract with borrow
  | XRI
  | CPI
  deriving (Eq,Ord,Show)

-- | Ops which take two immediate bytes.
data Op2
  = LXI RegPairSpec
  | SHLD
  | STA
  | LHLD
  | LDA
  | JCond Condition
  | JMP
  | JMPx
  | CCond Condition
  | CALL
  | CALLx Word8 -- (0..3)
  deriving (Eq,Ord,Show)

data Condition = NZ | NC | PO | P | Z | CY | PE | MI
  deriving (Eq,Ord,Show)

data RegSpec = A | B | C | D | E | H | L | M
  deriving (Eq,Ord,Show)

data RegPairSpec = BC | DE | HL | SP | PSW
  deriving (Eq,Ord,Show)

allOps :: [Op]
allOps = map Op0 all0 ++ map Op1 all1 ++ map Op2 all2
  where
    all0 =
      [NOP,RLC,RAL,DAA,STC,RRC,RAR,CMA,CMC,HLT,XTHL,DI,RET,RETx,PCHL,SPHL,XCHG,EI]
      ++ [ op p | op <- [STAX,LDAX], p <- [BC,DE] ]
      ++ [ op r | op <- [INR,DCR,ADD,ADC,SUB,SBB,ANA,XRA,ORA,CMP], r <- regs ]
      ++ [ op p | op <- [INX,DAD,DCX], p <- rps1 ]
      ++ [ op p | op <- [POP,PUSH], p <- rps2 ]
      ++ [ MOV {dest,src} | dest <- regs, src <- regs, not (dest==M && src==M) ]
      ++ [ RCond c | c <- conds ]
      ++ [ RST n | n <- [0..7] ]
      ++ [ NOPx n | n <- [1..7] ]
    all1 =
      [ADI,SUI,ANI,ORI,ACI,SBI,XRI,CPI,OUT,IN] ++ [ MVI r | r <- regs ]
    all2 =
      [SHLD,STA,LHLD,LDA,JMP,JMPx,CALL]
      ++ [ LXI r | r <- rps1]
      ++ [ op c | op <- [CCond,JCond], c <- conds]
      ++ [ CALLx n | n <- [1..3] ]

    regs = [A,B,C,D,E,H,L,M]
    rps1 = [BC,DE,HL,SP]
    rps2 = [BC,DE,HL,PSW]
    conds= [Z,NZ,CY,NC,PO,PE,P,MI]


cycles :: Bool -> Op -> Int
cycles jumpTaken = \case
  Op0 NOP -> 4
  Op0 NOPx{} -> 4
  Op2 LXI{} -> 10
  Op0 STAX{} -> 7
  Op2 SHLD -> 16
  Op2 STA -> 13
  Op0 INX{} -> 5
  Op0 (INR r) -> mcost r 5 10
  Op0 (DCR r) -> mcost r 5 10
  Op1 (MVI r) -> mcost r 7 10
  Op0 RLC -> 4
  Op0 RAL -> 4
  Op0 DAA -> 4
  Op0 STC -> 4
  Op0 DAD{} -> 10 -- oops, I had a bug. I had 11
  Op0 LDAX{} -> 7
  Op2 LHLD -> 16
  Op2 LDA -> 13
  Op0 DCX{} -> 5
  Op0 RRC -> 4
  Op0 RAR -> 4
  Op0 CMA -> 4
  Op0 CMC -> 4
  Op0 HLT -> 7
  Op0 MOV {dest=M,src=M} -> error "illegal instruction: MOV M,M"
  Op0 MOV {src=M} -> 7
  Op0 MOV {dest=M} -> 7
  Op0 MOV{} -> 5
  Op0 (ADD r) -> mcost r 4 7
  Op0 (ADC r) -> mcost r 4 7
  Op0 (SUB r) -> mcost r 4 7
  Op0 (SBB r) -> mcost r 4 7
  Op0 (ANA r) -> mcost r 4 7
  Op0 (XRA r) -> mcost r 4 7
  Op0 (ORA r) -> mcost r 4 7
  Op0 (CMP r) -> mcost r 4 7
  Op0 RCond{} -> if jumpTaken then 11 else 5
  Op0 POP{} -> 10
  Op2 JCond{} -> 10
  Op2 JMP -> 10
  Op2 JMPx -> 10
  Op1 OUT -> 10
  Op0 XTHL -> 18
  Op0 DI -> 4
  Op2 CCond{} -> if jumpTaken then 17 else 11
  Op0 PUSH{} -> 11
  Op1 ADI -> 7
  Op1 SUI -> 7
  Op1 ANI -> 7
  Op1 ORI -> 7
  Op0 RST{} -> 4
  Op0 RET -> 10
  Op0 RETx -> 10
  Op0 PCHL -> 5
  Op0 SPHL -> 5
  Op1 IN -> 10
  Op0 XCHG -> 4 -- colourful reference table say 5, but is it wrong. The systems ref doc says 4.
  Op0 EI -> 4
  Op2 CALL -> 17
  Op2 CALLx{} -> 17
  Op1 ACI -> 7
  Op1 SBI -> 7
  Op1 XRI -> 7
  Op1 CPI -> 7

mcost :: RegSpec -> Int -> Int -> Int
mcost x a b = case x of M -> b; _ -> a

data Instruction b -- op+args
  = Ins0 Op0
  | Ins1 Op1 b
  | Ins2 Op2 b b
  deriving (Functor)

instance Show b => Show (Instruction b) where
  show i = prettyInstruction i

prettyInstructionBytes :: Show b => Instruction b -> String
prettyInstructionBytes i = unwords bytes
  where
    b0 = encode op
    op = justOp i
    bytes = case i of
      Ins0 _ -> [show b0]
      Ins1 _ b1 -> [show b0,show b1]
      Ins2 _ b1 b2 -> [show b0,show b1,show b2]

prettyInstruction :: Show b => Instruction b -> String
prettyInstruction = \case
  Ins0 NOP -> "NOP"
  Ins0 NOPx{} -> "*NOP"
  Ins2 (LXI rp) b1 b2 -> tag "LD" (show rp <> "," <> show b2 <> show b1)
  Ins2 SHLD b1 b2 -> tag "LD" ("(" <> show b2 <> show b1 <> "),HL")
  Ins2 STA b1 b2 -> tag "LD" ("("<> show b2 <> show b1 <> "),A")
  Ins0 (STAX rp) -> tag "LD" "(" <> show rp <> "),A"
  Ins0 (INX rp) -> tag "INC" (show rp)
  Ins0 (INR reg) -> tag "INC" (prettyReg reg)
  Ins0 (DCR reg) -> tag "DEC" (prettyReg reg)
  Ins1 (MVI dest) b1 -> tag "LD" (prettyReg dest <> "," <> show b1)
  Ins0 RLC -> "RLCA"
  Ins0 RAL -> "RAL"
  Ins0 DAA -> "DAA"
  Ins0 STC -> "SCF"
  Ins0 (DAD rp) -> tag "ADD" ("HL," <> show rp)
  Ins0 (LDAX rp) -> tag "LD" "A,(" <> show rp <> ")"
  Ins2 LHLD b1 b2 -> tag "LD" ("HL,(" <> show b2 <> show b1 <> ")")
  Ins2 LDA b1 b2 -> tag "LD" ("A,("<> show b2 <> show b1 <> ")")
  Ins0 (DCX rp) -> tag "DEC" (show rp)
  Ins0 RRC -> "RRCA"
  Ins0 RAR -> "RAR"
  Ins0 CMA -> "CPL"
  Ins0 CMC -> "CPC"
  Ins0 MOV {dest,src} -> tag "LD" (prettyReg dest <> "," <> prettyReg src)
  Ins0 HLT -> "HLT"
  Ins0 (ADD reg) -> tag "ADD" (prettyReg reg)
  Ins0 (ADC reg) -> tag "ADC" (prettyReg reg)
  Ins0 (SUB reg) -> tag "SUB" (prettyReg reg)
  Ins0 (SBB reg) -> tag "SBC" (prettyReg reg)
  Ins0 (ANA reg) -> tag "AND" (prettyReg reg)
  Ins0 (XRA reg) -> tag "XOR" (prettyReg reg)
  Ins0 (ORA reg) -> tag "OR" (prettyReg reg)
  Ins0 (CMP reg) -> tag "CP" (prettyReg reg)
  Ins0 (RCond cond) -> tag "RET" (show cond)
  Ins0 (POP rp) -> tag "POP" (show rp)
  Ins2 (JCond cond) b1 b2 -> tag "JP" (show cond <> "," <> show b2 <> show b1)
  Ins2 JMP b1 b2 -> tag "JP" (show b2 <> show b1)
  Ins2 JMPx b1 b2 -> tag "*JP" (show b2 <> show b1)
  Ins1 OUT b1 -> tag "OUT" (show b1)
  Ins0 XTHL -> tag "EX" "(SP),HL"
  Ins0 DI -> "DI"
  Ins2 (CCond cond) b1 b2 -> tag "CALL" (show cond <> "," <> show b2 <> show b1)
  Ins0 (PUSH rp) -> tag "PUSH" (show rp)
  Ins1 ADI b1 -> tag "ADD" (show b1)
  Ins1 SUI b1 -> tag "SUB" (show b1)
  Ins1 ANI b1 -> tag "AND" (show b1)
  Ins1 ORI b1 -> tag "OR" (show b1)
  Ins0 (RST n) -> tag "RST" (show n)
  Ins0 RET -> "RET"
  Ins0 RETx -> "*RET"
  Ins0 PCHL -> tag "JP" "(HL)"
  Ins0 SPHL -> tag "LD" "SP,HL"
  Ins1 IN b1 -> tag "IN" (show b1)
  Ins0 XCHG -> tag "EX" "DE,HL"
  Ins0 EI -> "EI"
  Ins2 CALL b1 b2 -> tag "CALL" (show b2 <> show b1)
  Ins2 CALLx{} b1 b2 -> tag "*CAL" (show b2 <> show b1)
  Ins1 ACI b1 -> tag "ADC" (show b1)
  Ins1 SBI b1 -> tag "SBC" (show b1)
  Ins1 XRI b1 -> tag "XOR" (show b1)
  Ins1 CPI b1 -> tag "CP" (show b1)
  where
    tag s more = ljust 5 s <> more

prettyReg :: RegSpec -> String
prettyReg = \case
  M -> "(HL)"
  reg -> show reg

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

justOp :: Instruction b -> Op
justOp = \case
  Ins0 op0 -> Op0 op0
  Ins1 op1 _ -> Op1 op1
  Ins2 op2 _ _ -> Op2 op2

encode :: Op -> Byte
encode = \case
  Op0 NOP -> 0x00
  Op0 (NOPx n) -> Byte (8 * n)
  Op2 (LXI rp) -> Byte (16 * encodeRegPairSpec rp + 0x1)
  Op2 SHLD -> 0x22
  Op2 STA -> 0x32
  Op0 (STAX rp) -> Byte (16 * encodeRegPairSpec rp + 0x2)
  Op0 (INX rp) -> Byte (16 * encodeRegPairSpec rp + 0x3)
  Op0 (INR reg) -> Byte (8 * encodeRegSpec reg + 0x04)
  Op0 (DCR reg) -> Byte (8 * encodeRegSpec reg + 0x05)
  Op1 (MVI dest) -> Byte (8 * encodeRegSpec dest + 0x06)
  Op0 RLC -> 0x07
  Op0 RAL -> 0x17
  Op0 DAA -> 0x27
  Op0 STC -> 0x37
  Op0 (DAD rp) -> Byte (16 * encodeRegPairSpec rp + 0x9)
  Op0 (LDAX rp) -> Byte (16 * encodeRegPairSpec rp + 0x0A)
  Op2 LHLD -> 0x2A
  Op2 LDA -> 0x3A
  Op0 (DCX rp) -> Byte (16 * encodeRegPairSpec rp + 0xB)
  Op0 RRC -> 0x0F
  Op0 RAR -> 0x1F
  Op0 CMA -> 0x2F
  Op0 CMC -> 0x3F
  Op0 MOV {dest,src} -> Byte (0x40 + 8 * encodeRegSpec dest + encodeRegSpec src)
  Op0 HLT -> 0x76
  Op0 (ADD reg) -> Byte (encodeRegSpec reg + 0x80)
  Op0 (ADC reg) -> Byte (encodeRegSpec reg + 0x88)
  Op0 (SUB reg) -> Byte (encodeRegSpec reg + 0x90)
  Op0 (SBB reg) -> Byte (encodeRegSpec reg + 0x98)
  Op0 (ANA reg) -> Byte (encodeRegSpec reg + 0xA0)
  Op0 (XRA reg) -> Byte (encodeRegSpec reg + 0xA8)
  Op0 (ORA reg) -> Byte (encodeRegSpec reg + 0xB0)
  Op0 (CMP reg) -> Byte (encodeRegSpec reg + 0xB8)
  Op0 (RCond cond) -> Byte (8 * encodeCondition cond + 0xC0)
  Op0 (POP rp) -> Byte (16 * encodeRegPairSpec rp + 0xC1)
  Op2 (JCond cond) -> Byte (8 * encodeCondition cond + 0xC2)
  Op2 JMP -> 0xC3
  Op2 JMPx -> 0xCB
  Op1 OUT -> 0xD3
  Op0 XTHL -> 0xE3
  Op0 DI -> 0xF3
  Op2 (CCond cond) -> Byte (8 * encodeCondition cond + 0xC4)
  Op0 (PUSH rp) -> Byte (16 * encodeRegPairSpec rp + 0xC5)
  Op1 ADI -> 0xC6
  Op1 SUI -> 0xD6
  Op1 ANI -> 0xE6
  Op1 ORI -> 0xF6
  Op0 (RST n) -> Byte (8 * fromIntegral n + 0xC7)
  Op0 RET -> 0xC9
  Op0 RETx -> 0xD9
  Op0 PCHL -> 0xE9
  Op0 SPHL -> 0xF9
  Op1 IN -> 0xDB
  Op0 XCHG -> 0xEB
  Op0 EI -> 0xFB
  Op2 CALL -> 0xCD
  Op2 (CALLx n) -> Byte (16 * n + 0xCD)
  Op1 ACI -> 0xCE
  Op1 SBI -> 0xDE
  Op1 XRI -> 0xEE
  Op1 CPI -> 0xFE

encodeCondition :: Condition -> Word8
encodeCondition = \case
  NZ -> 0
  Z -> 1
  NC -> 2
  CY -> 3
  PO -> 4
  PE -> 5
  P -> 6
  MI -> 7

encodeRegSpec :: RegSpec -> Word8
encodeRegSpec = \case
  B -> 0
  C -> 1
  D -> 2
  E -> 3
  H -> 4
  L -> 5
  M -> 6
  A -> 7

encodeRegPairSpec :: RegPairSpec -> Word8
encodeRegPairSpec = \case
  BC -> 0
  DE -> 1
  HL -> 2
  -- SP & PSW share same encoding
  SP -> 3
  PSW -> 3 -- for PUSH/POP

-- | define decode as the inverse of encoding
decode :: Byte -> Op
decode byte =
  case Map.lookup byte map of
    Nothing -> error $ "decode: " <> show byte
    Just op -> op
  where (DecodeTable map) = theDecodeTable

newtype DecodeTable = DecodeTable (Map Byte Op)

theDecodeTable :: DecodeTable
theDecodeTable = DecodeTable $ Map.fromList ys
  where
    xs = [ (encode op, op) | op <- allOps ]
    ys = [ (k,expectUnique k vs) | (k,vs) <- groupSort xs ]
    expectUnique k = \case
      [op] -> op
      ops -> error $
        unlines $
        ("bad decoding: " <> show k)
          : [ "--> " <> ljust 13 (show (docInstructionForOp op)) <> " [" <> show op <> "]" | op <- ops ]

instance Show DecodeTable where
  show (DecodeTable map) =
    unlines
    [ unlines [ show k <> " --> " <> show (docInstructionForOp v) | (k,v) <- ps ]
    , printf "implemented: %d, unimplemented %d" n (256-n)
    ]
    where
      ps = sort (Map.toList map)
      n = length ps

docInstructionForOp :: Op -> Instruction ImmSpec
docInstructionForOp = \case
  Op0 op0 -> Ins0 op0
  Op1 op1 -> Ins1 op1 B1
  Op2 op2 -> Ins2 op2 B1 B2

data ImmSpec = B1 | B2

instance Show ImmSpec where
  show = \case
    B1 -> "b1"
    B2 -> "b2"
