
module InstructionSet (
  Op(..),Op0(..),Op1(..),Op2(..), RegPairSpec(..), RegSpec(..),
  Instruction(..),
  cycles,
  decode,
  printDecodeTable,
  prettyInstructionBytes,
  ) where

import Text.Printf (printf)
import Data.List (sort)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Data.Word8 (Word8)
import Byte (Byte(..))
import qualified Data.Map.Strict as Map

data Op = Op0 Op0 | Op1 Op1 | Op2 Op2
  deriving (Eq,Ord)

data Op0
  = NOP
  | RET
  | RZ
  | RC
  | RNZ
  | RRC
  | RLC
  | RAR
  | EI
  | STC
  | XCHG
  | LDAX_B
  | LDAX_D
  | INR RegSpec
  | DCR RegSpec
  | XRA RegSpec
  | ANA RegSpec
  | ORA RegSpec
  | MOV { dest :: RegSpec, src :: RegSpec }
  | INX RegPairSpec
  | PUSH RegPairSpec
  | POP RegPairSpec
  | DAD RegPairSpec
  | RST Word8 --(0..7)
  deriving (Eq,Ord)

data Op1
  = CPI
  | OUT
  | IN
  | ANI
  | ORI
  | ADI
  | MVI RegSpec
  deriving (Eq,Ord)

data Op2
  = JP
  | JNZ
  | JNC
  | JZ
  | JC
  | CALL
  | CNZ
  | LDA
  | STA
  | LHLD
  | LXI RegPairSpec
  deriving (Eq,Ord)

data RegSpec = A | B | C | D | E | H | L | M
  deriving (Eq,Ord,Show)

data RegPairSpec = BC | DE | HL | SP | PSW
  deriving (Eq,Ord,Show)

allOps :: [Op]
allOps = map Op0 allOp0 ++ map Op1 allOp1 ++ map Op2 allOp2
  where
    allOp0 = [NOP,LDAX_B,LDAX_D ,RET,RZ,RC,RNZ,RRC,RLC,RAR,EI,STC,XCHG]
             ++ map INR regs7spec
             ++ map DCR regs7spec
             ++ map XRA regs7spec
             ++ map ANA regs7spec
             ++ map ORA regs7spec
             ++ map INX rps1 ++ map DAD rps1 ++ map PUSH rps2 ++ map POP rps2
             ++ map RST [0..7]
             ++ [ MOV {dest,src} | dest <- regs7spec, src <- regs7spec, not (dest==M && src==M) ]
    allOp1 = [CPI,OUT,IN,ANI,ORI,ADI] ++ map MVI regs7spec
    allOp2 = [JP,JNZ,JNC,JZ,JC,CALL,CNZ,LDA,STA,LHLD] ++ map LXI rps1
    regs7spec = [A,B,C,D,E,H,L,M]
    rps1 = [BC,DE,HL,SP]
    rps2 = [BC,DE,HL,PSW]


cycles :: Bool -> Op -> Int
cycles jumpTaken = \case
  Op0 NOP -> 4
  Op0 RET -> 10
  Op0 RZ -> if jumpTaken then 11 else 5
  Op0 RC -> if jumpTaken then 11 else 5
  Op0 RNZ -> if jumpTaken then 11 else 5
  Op0 RRC -> 4
  Op0 RLC -> 4
  Op0 RAR -> 4
  Op0 EI -> 4
  Op0 STC -> 4
  Op0 XCHG -> 5
  Op0 LDAX_B -> 7
  Op0 LDAX_D -> 7
  Op0 (DCR M) -> 10
  Op0 DCR{} -> 5
  Op0 (INR M) -> 10
  Op0 INR{} -> 5
  Op0 XRA{} -> 4
  Op0 ANA{} -> 4
  Op0 (ORA M) -> 7
  Op0 ORA{} -> 4
  Op0 MOV {dest=M,src=M} -> error "illegal instruction: MOV M,M"
  Op0 MOV {src=M} -> 7
  Op0 MOV {dest=M} -> 7
  Op0 MOV{} -> 7 -- TODO: wrong? should be 5
  Op0 INX{} -> 5
  Op0 PUSH{} -> 11
  Op0 POP{} -> 10
  Op0 DAD{} -> 11
  Op0 RST{} -> 4
  Op1 (MVI M) -> 10
  Op1 MVI{} -> 7
  Op1 CPI -> 7
  Op1 OUT -> 10
  Op1 IN -> 10
  Op1 ANI -> 7
  Op1 ORI -> 7
  Op1 ADI -> 7
  Op2 JP -> 10
  Op2 JNZ -> 10
  Op2 JNC -> 10
  Op2 JZ -> 10
  Op2 JC -> 10
  Op2 CALL -> 17
  Op2 CNZ -> if jumpTaken then 17 else 11
  Op2 LDA -> 13
  Op2 STA -> 13
  Op2 LHLD -> 16
  Op2 LXI{} -> 10


data Instruction b -- op+args
  = Ins0 Op0
  | Ins1 Op1 b
  | Ins2 Op2 b b

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
  Ins0 RET -> "RET"
  Ins0 RZ -> tag "RET" "Z"
  Ins0 RC -> tag "RET" "C"
  Ins0 RNZ -> tag "RET" "NZ"
  Ins0 RRC -> tag "RRCA" ""
  Ins0 RLC -> tag "RLCA" ""
  Ins0 RAR -> "RAR"
  Ins0 EI -> "EI"
  Ins0 STC -> "SCF"
  Ins0 XCHG -> tag "EX" "DE,HL"
  Ins0 LDAX_B -> tag "LD" "A,(BC)"
  Ins0 LDAX_D -> tag "LD" "A,(DE)"
  Ins0 (INR reg) -> tag "INC" (prettyReg reg)
  Ins0 (DCR reg) -> tag "DEC" (prettyReg reg)
  Ins0 (XRA reg) -> tag "XOR" (prettyReg reg)
  Ins0 (ANA reg) -> tag "AND" (prettyReg reg)
  Ins0 (ORA reg) -> tag "OR" (prettyReg reg)
  Ins0 MOV {dest,src} -> tag "LD" (prettyReg dest <> "," <> prettyReg src)
  Ins0 (INX rp) -> tag "INC" (show rp)
  Ins0 (PUSH rp) -> tag "PUSH" (show rp)
  Ins0 (POP rp) -> tag "POP" (show rp)
  Ins0 (DAD rp) -> tag "ADD" ("HL," <> show rp)
  Ins0 (RST n) -> tag "RST" (show n)
  Ins1 (MVI dest) b1 -> tag "LD" (prettyReg dest <> "," <> show b1)
  Ins1 CPI b1 -> tag "CP" (show b1)
  Ins1 OUT b1 -> tag "OUT" (show b1)
  Ins1 IN b1 -> tag "IN" (show b1)
  Ins1 ANI b1 -> tag "AND" (show b1)
  Ins1 ORI b1 -> tag "OR" (show b1)
  Ins1 ADI b1 -> tag "ADD" (show b1)
  Ins2 JP b1 b2 -> tag "JP" (show b2 <> show b1)
  Ins2 JNZ b1 b2 -> tag "JP" ("NZ," <> show b2 <> show b1)
  Ins2 JNC b1 b2 -> tag "JP" ("NC," <> show b2 <> show b1)
  Ins2 JZ b1 b2 -> tag "JP" ("Z," <> show b2 <> show b1)
  Ins2 JC b1 b2 -> tag "JP" ("C," <> show b2 <> show b1)
  Ins2 CALL b1 b2 -> tag "CALL" (show b2 <> show b1)
  Ins2 CNZ b1 b2 -> tag "CALL" ("NZ," <> show b2 <> show b1)
  Ins2 LDA b1 b2 -> tag "LD" ("A,("<> show b2 <> show b1 <> ")")
  Ins2 STA b1 b2 -> tag "LD" ("("<> show b2 <> show b1 <> "),A")
  Ins2 LHLD b1 b2 -> tag "LD" ("HL," <> show b2 <> show b1)
  Ins2 (LXI rp) b1 b2 -> tag "LD" (show rp <> "," <> show b2 <> show b1)
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
  Op0 RET -> 0xC9
  Op0 RZ -> 0xC8
  Op0 RC -> 0xD8
  Op0 RNZ -> 0xC0
  Op0 RRC -> 0x0F
  Op0 RLC -> 0x07
  Op0 RAR -> 0x1F
  Op0 EI -> 0xFB
  Op0 STC -> 0x37
  Op0 XCHG -> 0xEB
  Op0 LDAX_B -> 0x0A
  Op0 LDAX_D -> 0x1A
  Op0 (DCR reg) -> Byte (8 * encodeRegSpec reg + 0x05)
  Op0 (INR reg) -> Byte (8 * encodeRegSpec reg + 0x04)
  Op0 (XRA reg) -> Byte (encodeRegSpec reg + 0xA8)
  Op0 (ANA reg) -> Byte (encodeRegSpec reg + 0xA0)
  Op0 (ORA reg) -> Byte (encodeRegSpec reg + 0xB0)
  Op0 MOV {dest,src} -> Byte (0x40 + 8 * encodeRegSpec dest + encodeRegSpec src)
  Op0 (INX rp) -> Byte (16 * encodeRegPairSpec rp + 0x3)
  Op0 (PUSH rp) -> Byte (16 * encodeRegPairSpec rp + 0xC5)
  Op0 (POP rp) -> Byte (16 * encodeRegPairSpec rp + 0xC1)
  Op0 (DAD rp) -> Byte (16 * encodeRegPairSpec rp + 0x9)
  Op0 (RST n) -> Byte (8 * fromIntegral n + 0xC7)
  Op1 (MVI dest) -> Byte (8 * encodeRegSpec dest + 0x06)
  Op1 CPI -> 0xFE
  Op1 OUT -> 0xD3
  Op1 IN -> 0xDB
  Op1 ANI -> 0xE6
  Op1 ORI -> 0xF6
  Op1 ADI -> 0xC6
  Op2 JP -> 0xC3
  Op2 JNZ -> 0xC2
  Op2 JNC -> 0xD2
  Op2 JZ -> 0xCA
  Op2 JC -> 0xDA
  Op2 CALL -> 0xCD
  Op2 CNZ -> 0xC4
  Op2 LDA -> 0x3A
  Op2 STA -> 0x32
  Op2 LHLD -> 0x2A
  Op2 (LXI rp) -> Byte (16 * encodeRegPairSpec rp + 0x1)

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
decode :: Byte -> Maybe Op
decode byte = Map.lookup byte decodeTable

decodeTable :: Map Byte Op
decodeTable = Map.fromList ys
  where
    xs = [ (encode op, op) | op <- allOps ]
    ys = [ (k,expectUnique k vs) | (k,vs) <- groupSort xs ]
    expectUnique k = \case
      [op] -> op
      ops -> error $
        unlines $
        ("bad decoding: " <> show k)
          : [ "--> " <> show (docInstructionForOp op) | op <- ops ]

printDecodeTable :: IO ()
printDecodeTable = do
  putStrLn $ unlines [ show k <> " --> " <> show (docInstructionForOp v) | (k,v) <- ps ]
  putStrLn (printf "implemented: %d, unimplemented %d" n (256-n))
  where
    ps = sort (Map.toList decodeTable)
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
