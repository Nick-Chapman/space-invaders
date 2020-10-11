
module InstructionSet (
  Op(..),Op0(..),Op1(..),Op2(..),
  Instruction(..),
  decode,
  printDecodeTable
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
  | LDAX_D
  | DCR Reg
  | RET
  | MOV_M_A
  | MOV_E_M
  | MOV Reg Reg
  | INX RegPair
  | PUSH RegPair
  | POP RegPair
  | DAD RegPair
  | XCHG
  deriving (Eq,Ord,Show)

data Op1
  = CPI
  | OUT
  | MVI_M
  | MVI Reg
  deriving (Eq,Ord,Show)

data Op2
  = JP
  | JNZ
  | CALL
  | LXI RegPair
  deriving (Eq,Ord,Show)

allOps :: [Op]
allOps = map Op0 allOp0 ++ map Op1 allOp1 ++ map Op2 allOp2
  where
    allOp0 = [NOP,LDAX_D
             ,MOV_M_A
             ,MOV_E_M
             ,RET,XCHG]
             ++ map DCR regs7
             ++ map INX rps1 ++ map DAD rps1 ++ map PUSH rps2 ++ map POP rps2
             ++ [ MOV dest src | dest <- regs7, src <- regs7 ]
    allOp1 = [CPI,OUT,MVI_M] ++ map MVI regs7
    allOp2 = [JP,JNZ,CALL] ++ map LXI rps1
    regs7 = [A,B,C,D,E,H,L]
    rps1 = [BC,DE,HL,SP]
    rps2 = [BC,DE,HL,PSW]

data Instruction b -- op+args
  = Ins0 Op0 b
  | Ins1 Op1 b b
  | Ins2 Op2 b b b

instance Show b => Show (Instruction b) where
  show i =
    ljust 10 (unwords (map show bytes))
    <> ljust 11 (brace (show (justOp i)))
    <> prettyInstruction i
    where
      bytes = case i of
        Ins0 _ b0 -> [b0]
        Ins1 _ b0 b1 -> [b0,b1]
        Ins2 _ b0 b1 b2 -> [b0,b1,b2]

prettyInstruction :: Show b => Instruction b -> String
prettyInstruction = \case
  Ins0 NOP _ -> "NOP"
  Ins0 MOV_M_A _ -> tag "LD" "(HL),A"
  Ins0 MOV_E_M _ -> tag "LD" "E,(HL)"
  Ins0 (MOV dest src) _ -> tag "LD" (show dest <> "," <> show src)
  Ins0 (INX rp) _ -> tag "INC" (show rp)
  Ins0 (PUSH rp) _ -> tag "PUSH" (show rp)
  Ins0 (POP rp) _ -> tag "POP" (show rp)
  Ins0 (DAD rp) _ -> tag "ADD" ("HL," <> show rp)
  Ins0 XCHG _ -> tag "EX" "DE,HL"
  Ins0 (DCR reg) _ -> tag "DEC" (show reg)
  Ins0 RET _ -> "RET"
  Ins0 LDAX_D _ -> tag "LD" "A,(DE)"
  Ins1 (MVI dest) _ b1 -> tag "LD" (show dest <> "," <> show b1)
  Ins1 MVI_M _ b1 -> tag "LD" ("(HL)" <> "," <> show b1)
  Ins1 CPI _ b1 -> tag "CP" (show b1)
  Ins1 OUT _ b1 -> tag "OUT" (show b1)
  Ins2 JP _ b1 b2 -> tag "JP" (show b2 <> show b1)
  Ins2 JNZ _ b1 b2 -> tag "JNZ" (show b2 <> show b1)
  Ins2 CALL _ b1 b2 -> tag "CALL" (show b2 <> show b1)
  Ins2 (LXI rp) _ b1 b2 -> tag "LD" (show rp <> "," <> show b2 <> show b1)
  where
    tag s more = ljust 5 s <> more

brace :: String -> String
brace s = "(" <> s <> ")"

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

instance Show Op where
  show = \case
    Op0 x -> show x
    Op1 x -> show x
    Op2 x -> show x


justOp :: Instruction b  -> Op
justOp = \case
  Ins0 op0 _ -> Op0 op0
  Ins1 op1 _ _ -> Op1 op1
  Ins2 op2 _ _ _ -> Op2 op2

encode :: Op -> Byte
encode = \case
  Op0 NOP -> 0x00
  Op0 MOV_M_A -> 0x77 -- TODO: gen
  Op0 MOV_E_M -> 0x5E -- TODO: gen
  Op0 (MOV dest src) -> Byte (64 + 8 * encodeReg7 dest + encodeReg7 src)
  Op0 (INX rp) -> Byte (16 * encodeRegPair rp + 0x3)
  Op0 (PUSH rp) -> Byte (16 * encodeRegPair rp + 0xC5)
  Op0 (POP rp) -> Byte (16 * encodeRegPair rp + 0xC1)
  Op0 (DAD rp) -> Byte (16 * encodeRegPair rp + 0x9)
  Op0 XCHG -> 0xEB
  Op0 LDAX_D -> 0x1A
  Op0 (DCR reg) -> Byte (8 * encodeReg7 reg + 0x05)
  Op0 RET -> 0xC9
  Op1 (MVI dest) -> Byte (8 * encodeReg7 dest + 0x06)
  Op1 MVI_M -> 0x36
  Op1 CPI -> 0xFE
  Op1 OUT -> 0xD3
  Op2 JP -> 0xC3
  Op2 JNZ -> 0xC2
  Op2 CALL -> 0xCD
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
      [v] -> v
      vs -> error $ "bad decoding: " <> show k <> " --> " <> show vs

printDecodeTable :: IO ()
printDecodeTable = do
  putStrLn $ unlines [ show k <> " --> " <> show v | (k,v) <- ps ]
  putStrLn (printf "implemented: %d, unimplemented %d" n (256-n))
  where
    ps = sort (Map.toList decodeTable)
    n = length ps
