
module InstructionSet (Op(..),Op0(..),Op1(..),Op2(..),decode) where

import Addr (Addr)
import Byte (Byte)

data Op0
  = NOP
  deriving Show

data Op1
  = MVI_B
  deriving Show

data Op2
  = JP
  | LXI_SP
  | LXI_DE
  | CALL
  deriving Show

data Op = Op0 Op0 | Op1 Op1 | Op2 Op2

--encode :: Op -> Byte -- TODO derive decode by reversing encode

decode :: Addr -> Byte -> Op
decode at = \case
  0x00 -> Op0 NOP
  0xC3 -> Op2 JP
  0x31 -> Op2 LXI_SP
  0x11 -> Op2 LXI_DE
  0x06 -> Op1 MVI_B
  0xCD -> Op2 CALL
  byte ->
    error $ "decode(at: " <> show at <> ") : " <> show byte
