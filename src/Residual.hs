
module Residual
  ( CompTime
  , Program(..)
  , Exp17(..)
  , Exp16(..)
  , Exp8(..)
  , Exp1(..)
  , AVar(..)
  , Lay, layOpPrograms,layPrograms, lay, vert, tab
  ) where

import Addr (Addr)
import Buttons (But)
import Byte (Byte(..))
import Cpu -- (Reg(..),Flag(..))
import Data.Word8 (Word8)
import HiLo (HiLo(..))
import InstructionSet (Op,Instruction,encode)
import Phase (Phase)
import Sounds (Sound)
import qualified Phase (Bit,Byte,Addr)
import qualified Shifter

data CompTime

instance Phase CompTime where
  type Bit CompTime = Exp1
  type Byte CompTime = Exp8
  type Addr CompTime = Exp16

data Program
  = S_Jump Exp16
  | S_If Exp1 Program Program
  | S_Switch8 Exp8 [(Word8,Program)]
  | S_AssignReg16 Reg16 Exp16 Program
  | S_AssignReg Reg Exp8 Program
  | S_AssignShifterReg Shifter.Reg Exp8 Program
  | S_AssignFlag Flag Exp1 Program
  | S_MemWrite Exp16 Exp8 Program
  | S_Let16 AVar Exp16 Program
  | S_Let8 AVar Exp8 Program
  | S_Let17 AVar Exp17 Program
  | S_AtRef Addr Program
  | S_MarkReturnAddress Exp16 Program
  | S_TraceInstruction (Cpu CompTime) (Instruction Exp8) Program
  | S_Advance Int Program
  | S_UnknownOutput Word8 Exp8 Program
  | S_SoundControl Sound Exp1 Program
  | S_EnableInterrupts Program
  | S_DisableInterrupts Program

data Exp17
  = E17_Add Exp16 Exp16
  | E17_Var AVar
  deriving (Eq)

data Exp1
  = E1_False
  | E1_True
  | E1_Flag Flag
  | E1_TestBit Exp8 Int
  | E1_Flip Exp1
  | E1_AndBit Exp1 Exp1
  | E1_OrBit Exp1 Exp1
  | E1_IsZero Exp8
  | E1_IsParity Exp8
  | E1_HiBitOf17 Exp17
  | E1_Button But
  deriving (Eq)

-- TODO: have Exp9, for result of 8-bit add-with-carry

data Exp8
  = E8_Lit Byte
  | E8_Reg Reg
  | E8_ShifterReg Shifter.Reg
  | E8_Hi Exp16
  | E8_Lo Exp16
  | E8_ReadMem Exp16
  | E8_UpdateBit Exp8 Int Exp1
  | E8_Complement Exp8
  | E8_AndB Exp8 Exp8
  | E8_OrB Exp8 Exp8
  | E8_XorB Exp8 Exp8
  | E8_ShiftRight Exp8 Exp8
  | E8_ShiftLeft Exp8 Exp8
  | E8_Ite Exp1 Exp8 Exp8
  | E8_Var AVar
  | E8_UnknownInput Word8
  deriving (Eq)

data Exp16
  = E16_HiLo (HiLo Exp8)
  | E16_OffsetAdr Int Exp16
  | E16_Reg Reg16
  | E16_Var AVar
  | E16_AddWithCarry Exp1 Exp8 Exp8 -- TODO: This should construct E9
  | E16_DropHiBitOf17 Exp17
  | E16_Lit Addr
  deriving (Eq)

newtype AVar = AVar { u :: Int } -- address (16bit) variable
  deriving (Eq,Ord)

instance Show AVar where show AVar{u} = "a" ++ show u

instance Show Program where show = show . layProgram

layProgram :: Program -> Lay
layProgram = \case
  S_Jump a -> lay ("jump " ++ parenthesize (show a) ++ ";")
  S_If e s1 s2 ->
    vert [ lay ("if " ++ parenthesize (show e) ++ " {")
         , tab (layProgram s1)
         , lay "} else {"
         , tab (layProgram s2)
         , lay "}"
         ]
  S_Switch8 exp branches ->
    vert [ lay ("switch " ++ parenthesize (show exp) ++ " {")
         , tab (vert [ vert [ lay ("case " ++ show v ++ " : {")
                            , tab (layProgram p)
                            , lay "}"
                            ]
                     | (v,p) <- branches
                     ])
         , lay "}"
         ]

  S_AssignReg16 reg exp next ->
    vert [ lay (show reg ++ " := " ++ show exp ++ ";")
         , layProgram next
         ]
  S_AssignReg reg exp next ->
    vert [ lay (show reg ++ " := " ++ show exp ++ ";")
         , layProgram next
         ]
  S_AssignShifterReg reg exp next ->
    vert [ lay (show reg ++ " := " ++ show exp ++ ";")
         , layProgram next
         ]
  S_AssignFlag flag exp1 next ->
    vert [ lay (show flag ++ " := " ++ show exp1 ++ ";")
         , layProgram next
         ]
  S_MemWrite lv rv next ->
    vert [ lay ("M[" ++ show lv ++ "] := " ++ show rv ++ ";")
         , layProgram next
         ]
  S_Let16 v e next ->
    vert [ lay ("let:16 " ++ show v ++ " = " ++ show e ++ " in")
         , layProgram next
         ]
  S_Let8 v e next ->
    vert [ lay ("let:8 " ++ show v ++ " = " ++ show e ++ " in")
         , layProgram next
         ]
  S_Let17 v e next ->
    vert [ lay ("let:17 " ++ show v ++ " = " ++ show e ++ " in")
         , layProgram next
         ]
  S_AtRef pc next ->
    vert [ lay ("#" ++ show pc)
         , layProgram next
         ]
  S_MarkReturnAddress a next ->
    vert [ lay ("#return-to: " ++ show a)
         , layProgram next
         ]
  S_TraceInstruction _cpu i next ->
    vert [ lay ("#instruction: " ++ show i)
         , layProgram next
         ]
  S_Advance n next ->
    vert [ lay ("advance " ++ parenthesize (show n))
         , layProgram next
         ]
  S_UnknownOutput port byte next ->
    vert [ lay ("unknown_output" ++ show (port, byte) ++ ";")
         , layProgram next
         ]
  S_SoundControl sound bool next ->
    vert [ lay ("sound_control" ++ show (sound,bool) ++ ";")
         , layProgram next
         ]
  S_EnableInterrupts next ->
    vert [ lay "enable_interrupts()"
         , layProgram next
         ]
  S_DisableInterrupts next ->
    vert [ lay "disable_interrupts()"
         , layProgram next
         ]

instance Show Exp1 where
  show = \case
    E1_False -> "false"
    E1_True -> "true"
    E1_Flag flag -> show flag
    E1_TestBit e i -> show e ++ "[" ++ show i ++ "]"
    E1_Flip p -> "!" ++ show p
    E1_AndBit e1 e2 -> parenthesize (show e1 ++ " && " ++ show e2)
    E1_OrBit e1 e2 -> parenthesize (show e1 ++ " || " ++ show e2)
    E1_IsZero e -> "is_zero" ++ parenthesize (show e)
    E1_IsParity e -> "parity" ++ parenthesize (show e)
    E1_HiBitOf17 e -> show e ++ "[16]"
    E1_Button but -> "is_pressed" ++ parenthesize (show but)

instance Show Exp8 where
  show = \case
    E8_Lit x -> show x
    E8_Reg reg -> show reg
    E8_ShifterReg reg -> show reg
    E8_Hi a -> show a ++ "[15:8]"
    E8_Lo a -> show a ++ "[7:0]"
    E8_ReadMem a -> "M[" ++ show a ++ "]"
    E8_UpdateBit e i p -> "updateBit" ++ show (e,i,p)
    E8_Complement e -> "~" ++ show e
    E8_AndB e1 e2 -> parenthesize (show e1 ++ " & " ++ show e2)
    E8_OrB e1 e2 -> parenthesize (show e1 ++ " | " ++ show e2)
    E8_XorB e1 e2 -> parenthesize (show e1 ++ " ^ " ++ show e2)
    E8_ShiftRight e1 e2 -> parenthesize (show e1 ++ " >> " ++ show e2)
    E8_ShiftLeft e1 e2 -> parenthesize (show e1 ++ " << " ++ show e2)
    E8_Ite i t e -> parenthesize (show i ++ " ? " ++ show t ++ " : " ++ show e)
    E8_Var v -> show v
    E8_UnknownInput port -> "unknown_input" ++ parenthesize (show port)

instance Show Exp16 where
  show = \case
    E16_HiLo HiLo{hi,lo} ->
      parenthesize (show hi ++ "," ++ show lo)
    E16_OffsetAdr n e ->
      parenthesize (show n ++ " + " ++ show e)
    E16_Var v ->
      show v
    E16_Reg v ->
      show v
    E16_AddWithCarry cin e1 e2 ->
      "addWithCarry" ++ show (cin,e1,e2)
    E16_DropHiBitOf17 e ->
      show e ++ "[15:0]"
    E16_Lit x ->
      show x

instance Show Exp17 where
  show = \case
    E17_Add e1 e2 ->
      "add17" ++ show (e1,e2)
    E17_Var var ->
      show var

parenthesize :: String -> String
parenthesize x = "(" ++ x ++ ")"


data Lay = Lay { unLay :: [String] }

instance Show Lay where show = Prelude.unlines . unLay

lay :: String -> Lay
lay s = Lay [s]

vert :: [Lay] -> Lay
vert = Lay . concat . map unLay

tab :: Lay -> Lay
tab (Lay lines) = Lay $ [ "  " ++ line | line <- lines ]


layOpPrograms :: [(Op,Program)] -> Lay
layOpPrograms =
  layTagged (\op -> lay (show (encode op) ++ " --> " ++ show op)) (tab . layProgram)

layPrograms :: [(Addr,Program)] -> Lay
layPrograms =
  layTagged (\addr -> lay (show addr ++ ":")) (tab . layProgram)


layTagged :: (k -> Lay) -> (v -> Lay) -> [(k,v)] -> Lay
layTagged layK layV kvs = vert [ vert [lay "", layK k, lay "" , tab (layV v)] | (k,v) <- kvs ]
