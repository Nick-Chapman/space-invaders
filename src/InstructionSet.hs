
module InstructionSet (
  Op(..),Op0(..),Op1(..),Op2(..),
  Instruction(..),
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
import Cpu (Reg(..),RegPair(..))
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
  | EI
  | STC
  | XCHG
  | LDAX_B
  | LDAX_D
  | MOV_M_A
  | MOV_rM Reg
  | DCR Reg
  | DCR_M
  | XRA Reg
  | ANA Reg
  | ORA Reg
  | ORA_M
  | MOV Reg Reg
  | INX RegPair
  | PUSH RegPair
  | POP RegPair
  | DAD RegPair
  | RST Word8 --(0..7)
  deriving (Eq,Ord)

data Op1
  = CPI
  | OUT
  | IN
  | ANI
  | ADI
  | MVI_M
  | MVI Reg
  deriving (Eq,Ord)

data Op2
  = JP
  | JNZ
  | JNC
  | JZ
  | JC
  | CALL
  | LDA
  | STA
  | LXI RegPair
  deriving (Eq,Ord)


allOps :: [Op]
allOps = map Op0 allOp0 ++ map Op1 allOp1 ++ map Op2 allOp2
  where
    allOp0 = [NOP,LDAX_B,LDAX_D
             ,MOV_M_A
             ,RET,RZ,RC,RNZ,RRC,EI,STC,XCHG,DCR_M,ORA_M]
             ++ map MOV_rM regs7
             ++ map DCR regs7
             ++ map XRA regs7
             ++ map ANA regs7
             ++ map ORA regs7
             ++ map INX rps1 ++ map DAD rps1 ++ map PUSH rps2 ++ map POP rps2
             ++ map RST [0..7]
             ++ [ MOV dest src | dest <- regs7, src <- regs7 ]
    allOp1 = [CPI,OUT,IN,ANI,ADI,MVI_M] ++ map MVI regs7
    allOp2 = [JP,JNZ,JNC,JZ,JC,CALL,LDA,STA] ++ map LXI rps1
    regs7 = [A,B,C,D,E,H,L]
    rps1 = [BC,DE,HL,SP]
    rps2 = [BC,DE,HL,PSW]

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
  Ins0 RRC -> "RRCA"
  Ins0 EI -> "EI"
  Ins0 STC -> "SCF"
  Ins0 XCHG -> tag "EX" "DE,HL"
  Ins0 LDAX_B -> tag "LD" "A,(BC)"
  Ins0 LDAX_D -> tag "LD" "A,(DE)"
  Ins0 MOV_M_A -> tag "LD" "(HL),A"
  Ins0 (MOV_rM reg) -> tag "LD" (show reg <> ",(HL)")
  Ins0 (DCR reg) -> tag "DEC" (show reg)
  Ins0 DCR_M -> tag "DEC" "(HL)"
  Ins0 (XRA reg) -> tag "XOR" (show reg)
  Ins0 (ANA reg) -> tag "AND" (show reg)
  Ins0 (ORA reg) -> tag "OR" (show reg)
  Ins0 ORA_M -> tag "OR" "M"
  Ins0 (MOV dest src) -> tag "LD" (show dest <> "," <> show src)
  Ins0 (INX rp) -> tag "INC" (show rp)
  Ins0 (PUSH rp) -> tag "PUSH" (show rp)
  Ins0 (POP rp) -> tag "POP" (show rp)
  Ins0 (DAD rp) -> tag "ADD" ("HL," <> show rp)
  Ins0 (RST n) -> tag "RST" (show n)
  Ins1 (MVI dest) b1 -> tag "LD" (show dest <> "," <> show b1)
  Ins1 MVI_M b1 -> tag "LD" ("(HL)" <> "," <> show b1)
  Ins1 CPI b1 -> tag "CP" (show b1)
  Ins1 OUT b1 -> tag "OUT" (show b1)
  Ins1 IN b1 -> tag "IN" (show b1)
  Ins1 ANI b1 -> tag "AND" (show b1)
  Ins1 ADI b1 -> tag "ADD" (show b1)
  Ins2 JP b1 b2 -> tag "JP" (show b2 <> show b1)
  Ins2 JNZ b1 b2 -> tag "JP" ("NZ," <> show b2 <> show b1)
  Ins2 JNC b1 b2 -> tag "JP" ("NC," <> show b2 <> show b1)
  Ins2 JZ b1 b2 -> tag "JP" ("Z," <> show b2 <> show b1)
  Ins2 JC b1 b2 -> tag "JP" ("C," <> show b2 <> show b1)
  Ins2 CALL b1 b2 -> tag "CALL" (show b2 <> show b1)
  Ins2 LDA b1 b2 -> tag "LD" ("A,("<> show b2 <> show b1 <> ")")
  Ins2 STA b1 b2 -> tag "LD" ("("<> show b2 <> show b1 <> "),A")
  Ins2 (LXI rp) b1 b2 -> tag "LD" (show rp <> "," <> show b2 <> show b1)
  where
    tag s more = ljust 5 s <> more

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
  Op0 EI -> 0xFB
  Op0 STC -> 0x37
  Op0 XCHG -> 0xEB
  Op0 LDAX_B -> 0x0A
  Op0 LDAX_D -> 0x1A
  Op0 MOV_M_A -> 0x77 -- TODO: gen
  Op0 (MOV_rM reg) -> Byte (8 * encodeReg7 reg + 0x46)
  Op0 (DCR reg) -> Byte (8 * encodeReg7 reg + 0x05)
  Op0 DCR_M -> 0x35
  Op0 (XRA reg) -> Byte (encodeReg7 reg + 0xA8)
  Op0 (ANA reg) -> Byte (encodeReg7 reg + 0xA0)
  Op0 (ORA reg) -> Byte (encodeReg7 reg + 0xB0)
  Op0 ORA_M -> 0xB6
  Op0 (MOV dest src) -> Byte (0x40 + 8 * encodeReg7 dest + encodeReg7 src)
  Op0 (INX rp) -> Byte (16 * encodeRegPair rp + 0x3)
  Op0 (PUSH rp) -> Byte (16 * encodeRegPair rp + 0xC5)
  Op0 (POP rp) -> Byte (16 * encodeRegPair rp + 0xC1)
  Op0 (DAD rp) -> Byte (16 * encodeRegPair rp + 0x9)
  Op0 (RST n) -> Byte (8 * fromIntegral n + 0xC7)
  Op1 (MVI dest) -> Byte (8 * encodeReg7 dest + 0x06)
  Op1 MVI_M -> 0x36
  Op1 CPI -> 0xFE
  Op1 OUT -> 0xD3
  Op1 IN -> 0xDB
  Op1 ANI -> 0xE6
  Op1 ADI -> 0xC6
  Op2 JP -> 0xC3
  Op2 JNZ -> 0xC2
  Op2 JNC -> 0xD2
  Op2 JZ -> 0xCA
  Op2 JC -> 0xDA
  Op2 CALL -> 0xCD
  Op2 LDA -> 0x3A
  Op2 STA -> 0x32
  Op2 (LXI rp) -> Byte (16 * encodeRegPair rp + 0x1)

encodeReg7 :: Reg -> Word8
encodeReg7 = \case
  B -> 0
  C -> 1
  D -> 2
  E -> 3
  H -> 4
  L -> 5
--M -> 6
  A -> 7
  reg -> error $ "encodeReg7: " <> show reg

encodeRegPair :: RegPair -> Word8
encodeRegPair = \case
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
