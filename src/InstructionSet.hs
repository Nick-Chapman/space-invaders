
module InstructionSet (
  Op(..),Op0(..),Op1(..),Op2(..),
  Instruction(..),
  decode,
  prettyDecodeTable
  ) where

import Data.List (sort)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Data.Word8 (Word8)
import Addr (Addr)
import Byte (Byte(..))
import Cpu (RegPair(..))
import qualified Data.Map.Strict as Map

data Op = Op0 Op0 | Op1 Op1 | Op2 Op2
  deriving (Eq,Ord)

data Op0
  = NOP
  | LDAX_D
  | MVI_M_A
  | INX RegPair
  | DEC_B
  deriving (Eq,Ord,Show)

data Op1
  = MVI_B
  deriving (Eq,Ord,Show,Enum,Bounded)

data Op2
  = JP
  | CALL
  | LXI RegPair
  deriving (Eq,Ord,Show)

allOps :: [Op]
allOps = map Op0 allOp0 ++ map Op1 all ++ map Op2 allOp2
  where
    allOp0 = [NOP,LDAX_D,MVI_M_A,DEC_B] ++ map INX all
    allOp2 = [JP,CALL] ++ map LXI all
    all :: (Enum a, Bounded a) => [a]
    all = [minBound..maxBound]

data Instruction b -- op+args
  = Ins0 Op0 b
  | Ins1 Op1 b b
  | Ins2 Op2 b b b

instance Show b => Show (Instruction b) where
  show i =
    ljust 10 (unwords (map show bytes))
    <> ljust 10 (brace (show (justOp i)))
    <> prettyInstruction i
    where
      bytes = case i of
        Ins0 _ b0 -> [b0]
        Ins1 _ b0 b1 -> [b0,b1]
        Ins2 _ b0 b1 b2 -> [b0,b1,b2]

prettyInstruction :: Show b => Instruction b -> String
prettyInstruction = \case
  Ins0 NOP _ -> "NOP"
  Ins0 MVI_M_A _ -> tag "LD" "(HL),A"
  Ins0 (INX rp) _ -> tag "INC" (show rp)
  Ins0 DEC_B _ -> "DEC_B"
  Ins0 LDAX_D _ -> tag "LD" "A,(DE)"
  Ins1 MVI_B _ b1 -> tag "LD" ("B" <> "," <> show b1)
  Ins2 JP _ b1 b2 -> tag "JP" (show b2 <> show b1)
  Ins2 CALL _ b1 b2 -> tag "CALL" (show b2 <> show b1)
  Ins2 (LXI rp) _ b1 b2 -> tag "LD" (show rp <> "," <> show b2 <> show b1)
  where
    tag s more = ljust 6 s <> more

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
  Op0 MVI_M_A -> 0x77 -- TODO: gen
  Op0 (INX rp) -> Byte (16 * encodeRegPair rp + 0x3)
  Op0 LDAX_D -> 0x1A
  Op0 DEC_B -> 0x05
  Op1 MVI_B -> 0x06
  Op2 JP -> 0xC3
  Op2 CALL -> 0xCD
  Op2 (LXI rp) -> Byte (16 * encodeRegPair rp + 0x1)

encodeRegPair :: RegPair -> Word8
encodeRegPair = \case
  DE -> 1
  HL -> 2
  SP -> 3

-- | define decode as the inverse of encoding
decode :: Addr -> Byte -> Op
decode at byte =
  case Map.lookup byte decodeTable of
    Just op -> op
    Nothing ->
      error $ "decode(at: " <> show at <> ") : " <> show byte

decodeTable :: Map Byte Op
decodeTable = Map.fromList ys
  where
    xs = [ (encode op, op) | op <- allOps ]
    ys = [ (k,expectUnique k vs) | (k,vs) <- groupSort xs ]
    expectUnique k = \case
      [v] -> v
      vs -> error $ "bad decoding: " <> show k <> " --> " <> show vs

prettyDecodeTable :: [String]
prettyDecodeTable =
  [ show k <> " --> " <> show v | (k,v) <- sort (Map.toList decodeTable) ]
