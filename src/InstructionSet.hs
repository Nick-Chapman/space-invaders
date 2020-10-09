
module InstructionSet (Op(..),Op0(..),Op1(..),Op2(..),RP(..),Instruction(..),decode) where

import Data.Map (Map)
import Data.Word8 (Word8)
import Addr (Addr)
import Byte (Byte(..))
import qualified Data.Map.Strict as Map

data Op = Op0 Op0 | Op1 Op1 | Op2 Op2
  deriving Show

data Op0
  = NOP
  deriving (Show,Enum,Bounded)

data Op1
  = MVI_B
  deriving (Show,Enum,Bounded)

data Op2
  = JP
  | CALL
  | LXI RP
  deriving Show

-- | Register pair descriptor
data RP = SP | DE | HL
  deriving (Show,Enum,Bounded)

allOps :: [Op]
allOps = map Op0 all ++ map Op1 all ++ map Op2 allOp2
  where
    allOp2 = [JP,CALL] ++ map LXI all
    all :: (Enum a, Bounded a) => [a]
    all = [minBound..maxBound]

data Instruction b -- op+args
  = Ins0 Op0
  | Ins1 Op1 b
  | Ins2 Op2 b b

instance Show b => Show (Instruction b) where
  show = \case
    Ins0 op0 -> show op0
    Ins1 op1 b1 -> unwords [show op1, show b1]
    Ins2 op2 b1 b2 -> unwords [show op2, show b1, show b2]

encode :: Op -> Byte -- TODO derive decode by reversing encode
encode = \case
  Op0 NOP -> 0x00
  Op1 MVI_B -> 0x06
  Op2 JP -> 0xC3
  Op2 CALL -> 0xCD
  Op2 (LXI rp) -> Byte (16 * encodeRP rp + 0x1)

encodeRP :: RP -> Word8
encodeRP = \case
  DE -> 1
  HL -> 2
  SP -> 3

-- | define decode as the inverse of encoding
decode :: Addr -> Byte -> Op
decode at byte =
  case Map.lookup byte m of
    Just op -> op
    Nothing ->
      error $ "decode(at: " <> show at <> ") : " <> show byte
  where
    -- TODO: check/abort if encodings are not unique
    m :: Map Byte Op = Map.fromList [ (encode op, op) | op <- allOps ]
