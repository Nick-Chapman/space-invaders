
module InstructionSet (Op(..),decode) where

import Addr (Addr)
import Byte (Byte)

data Op
  = NOP
  | JP
  | LXI_SP
  deriving Show

--encode :: Op -> Byte -- TODO derive decode by reversing encode

decode :: Addr -> Byte -> Op
decode at = \case
  0x00 -> NOP
  0xC3 -> JP
  0x31 -> LXI_SP
  byte ->
    error $ "decode(at: " <> show at <> ") : " <> show byte
