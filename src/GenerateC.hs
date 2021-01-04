
module GenerateC (main) where

import Prelude hiding (init)

import Addr (Addr)
import Byte (Byte)
import Compile (compileAt)
import Cpu (Reg(..),Flag(..))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromJust)
import HiLo (HiLo(..))
import Residual (Exp1(..),Exp8(..),Exp16(..),Exp17(..),Program(..),AVar,Lay,vert,lay,tab)
import Rom (Rom,lookup)
import Static (oneStepReach,searchReach,startPoints)
import qualified Data.Map.Strict as Map
import qualified Rom (loadInvaders)
import qualified Shifter (Reg)

main :: IO ()
main = do
  putStrLn "*GenerateC*"
  rom <- Rom.loadInvaders
  cfile <- convertRom rom
  let fp = "c/program.c"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show cfile)

convertRom :: Rom -> IO CFile
convertRom rom = do
  let
    programsForEveryAddress =
      [ (addr,program)
      | addr <- [0..0x1FFF]
      , let program = compileAt (\_ -> False) rom addr ]

  let step :: Addr -> [Addr]
      step a = Map.findWithDefault (error $ "step: " <> show a) a stepMap
        where
          stepMap :: Map Addr [Addr]
          stepMap = Map.fromList [ (a, oneStepReach p) | (a,p) <- programsForEveryAddress ]

  reachSet <- searchReach step startPoints

  let reachablePrograms = [ (a,p) | (a,p) <- programsForEveryAddress, a `elem` reachSet ]
  let
    forwards = [FunDec $ makeForward addr | (addr,_) <- reachablePrograms ]
  let
    defs = [ FunDef $ convertProgramForAddress addr program
           | (addr,program) <- reachablePrograms ]
  let
    mem =
      ArrDef (CArrDef
               { typ=u8t
               , name=CName "mem"
               , size = Ident (CName "MEM_SIZE")
               , init = [ LitB $ fromJust $ Rom.lookup rom a
                        | a <- take 0x2000 [0..] ]
               })

  return $ CFile $ [ Include "<stdio.h>"
                   , Include "\"machine.h\""
                   ] ++ [mem] ++ forwards ++ defs

makeForward :: Addr -> CFunDec
makeForward addr =
  CFunDec { typ = control, name }
  where
    control = CType "Control"
    name = nameOfAddr addr

convertProgramForAddress :: Addr -> Program -> CFunDef
convertProgramForAddress addr program =
  CFunDef { typ = control, name, body }
  where
    control = CType "Control"
    name = nameOfAddr addr
    body = Block (convertProgram program)

nameOfAddr :: Addr -> CName
nameOfAddr a = CName ("prog_" ++ show a)

convertProgram :: Program  -> [CStat]
convertProgram = \case
  S_AtRef a next -> Expression (call "at" [LitS $ show a]) : convertProgram next
  S_MarkReturnAddress a next -> Comment ("#mark-return: " ++ show a) : convertProgram next
  S_TraceInstruction _cpu i pc next ->
    Expression (call "instruction" [LitS $ show i, LitA pc]) : convertProgram next
  S_Advance n next -> Expression (call "advance" [LitI n]) : convertProgram next
  S_Jump (E16_Lit a) -> [Return (call "jumpDirect" [Ident $ nameOfAddr a])]
  S_Jump e -> [Return (call "jump16" [convert16 e])]
  S_If i t e -> [If (convert1 i) (Block (convertProgram t)) (Block (convertProgram e))]
  S_AssignReg reg exp next -> Expression (Assign (convertReg reg) (convert8 exp)) : convertProgram next
  S_AssignFlag flag exp next -> Expression (Assign (convertFlag flag) (convert1 exp)) : convertProgram next
  S_AssignShifterReg{} -> todo "S_AssignShifterReg"
  S_MemWrite i e next -> Expression (call "mem_write" [convert16 i, convert8 e]) : convertProgram next
  S_Let17 v e next -> Declare u17t (convertVar v) (convert17 e) : convertProgram next
  S_Let16 v e next -> Declare u16t (convertVar v) (convert16 e) : convertProgram next
  S_Let8 v e next -> Declare u8t (convertVar v) (convert8 e) : convertProgram next
  S_SoundControl{} -> todo "S_SoundControl"
  S_EnableInterrupts{} -> todo "S_EnableInterrupts"
  S_DisableInterrupts{} -> todo "S_DisableInterrupts"
  S_UnknownOutput{} -> todo "S_UnknownOutput"

todo :: String -> [CStat]
todo s = [Expression $ call "todo" [LitS s], Die]

convertVar :: AVar -> CName
convertVar v = CName (show v)

u8t :: CType
u8t = CType "u8"

u16t :: CType
u16t = CType "u16"

u17t :: CType
u17t = CType "u17"

convert1 :: Exp1 -> CExp
convert1 = \case
  E1_Flag flag -> Ident (convertFlag flag)
  E1_True -> call "e1_true" []
  E1_False -> call "e1_false" []
  E1_Flip c -> call "e1_flip" [convert1 c]
  E1_IsZero b -> call "e1_is_zero" [convert8 b]
  E1_TestBit b i -> call "e1_test_bit" [convert8 b, LitI i]
  E1_HiBitOf17 x -> call "e1_hi_bit_of_17" [convert17 x]
  E1_IsParity b -> call "e1_parity" [convert8 b]
  E1_OrBit c1 c2 -> call "e1_or_bit" [convert1 c1, convert1 c2]
  E1_AndBit c1 c2 -> call "e1_and_bit" [convert1 c1, convert1 c2]
  E1_Button but -> call "e1_is_pressed" [LitS $ show but] -- TODO: use an enum for the buttons

convert8 :: Exp8 -> CExp
convert8 = \case
  E8_Lit b -> LitB b
  E8_Reg reg -> Ident (convertReg reg)
  E8_ShifterReg reg -> Ident (convertShifterReg reg)
  E8_Hi a -> call "e8_hi" [convert16 a]
  E8_Lo a -> call "e8_lo" [convert16 a]
  E8_ReadMem a -> call "e8_read_mem" [convert16 a]
  E8_UpdateBit e i p -> call "e8_update_bit" [convert8 e, LitI i, convert1 p]
  E8_Complement e -> call "e8_complement" [convert8 e]
  E8_AndB e1 e2 -> call "e8_and" [convert8 e1, convert8 e2]
  E8_OrB e1 e2 -> call "e8_or" [convert8 e1, convert8 e2]
  E8_XorB e1 e2 -> call "e8_xor" [convert8 e1, convert8 e2]
  E8_ShiftRight e1 e2 -> call "e8_shiftR" [convert8 e1, convert8 e2]
  E8_ShiftLeft e1 e2 -> call "e8_shiftL" [convert8 e1, convert8 e2]
  E8_Ite i t e -> call "e8_ite" [convert1 i, convert8 t, convert8 e]
  E8_Var v -> Ident (convertVar v)
  E8_UnknownInput{} -> undefined

convert16 :: Exp16 -> CExp
convert16 = \case
  E16_HiLo HiLo{hi,lo} -> call "e16_hi_lo" [convert8 hi, convert8 lo]
  E16_OffsetAdr n e -> call "e16_offset_addr" [LitI n, convert16 e]
  E16_Var v -> Ident (convertVar v)
  E16_AddWithCarry cin e1 e2 -> call "e16_add_with_carry" [convert1 cin, convert8 e1, convert8 e2]
  E16_DropHiBitOf17 e -> call "e16_drop_hi_bit_of_17" [convert17 e]
  E16_Lit x -> LitA x

convert17 :: Exp17 -> CExp
convert17 = \case
  E17_Var v -> Ident (convertVar v)
  E17_Add a1 a2 -> call "e17_add" [convert16 a1, convert16 a2]

call :: String -> [CExp] -> CExp
call s xs = Call (CName s) xs

convertShifterReg :: Shifter.Reg -> CName
convertShifterReg reg = CName (show reg)

convertReg :: Reg -> CName
convertReg reg = CName (show reg)

convertFlag :: Flag -> CName
convertFlag flag = CName (show flag)

data CFile = CFile [CTop]

data CTop
  = Include String
  | FunDef CFunDef
  | FunDec CFunDec
  | ArrDef CArrDef

data CFunDef = CFunDef
  { typ :: CType
  , name :: CName
  , body :: CStat
  }

data CFunDec = CFunDec
  { typ :: CType
  , name :: CName
  }

data CArrDef = CArrDef
  { typ :: CType
  , name :: CName
  , size :: CExp
  , init :: [CExp]
  }

data CType = CType String
data CName = CName String

data CStat
  = Printf String
  | Block [CStat]
  | Return CExp
  | Expression CExp
  | Comment String
  | Declare CType CName CExp
  | Die
  | If CExp CStat CStat

data CExp
  = LitI Int
  | LitS String
  | LitA Addr
  | LitB Byte
  | Ident CName
  | Call CName [CExp]
  | Assign CName CExp

instance Show CFile where
  show (CFile tops) = unlines (map show tops)

instance Show CTop where
  show = \case
    Include what -> unwords ["#include",what]
    FunDef def -> show def
    FunDec dec -> show dec
    ArrDef x -> show x

instance Show CFunDef where
  show CFunDef{typ,name,body} =
    show $ vert [ lay (unwords [show typ, show name, "()"])
                , layCStat body]

instance Show CFunDec where
  show CFunDec{typ,name} =
    show $ lay (unwords [show typ, show name, "();"])

instance Show CArrDef where
  show CArrDef{typ,name,size,init} =
    show $ lay (unwords ([show typ, show name, "[", show size, "] = {"]
                         ++ [intercalate "," (map show init)]
                         ++ ["};"]
                        ))

instance Show CType where show (CType s) = s

instance Show CName where show (CName s) = s

instance Show CStat where show = show . layCStat

layCStat :: CStat -> Lay
layCStat = \case
  Printf mes -> lay (unwords ["printf","(",dq++mes++dq,")",";"]) where dq = "\""
  Block xs -> brace (tab (vert [ layCStat x | x <- xs ]))
  Return e -> lay ("return " ++ show e ++ ";")
  Expression e -> lay (show e ++ ";")
  Comment s -> lay ("// " ++ s)
  Die -> lay "die;"
  Declare ty v e -> lay (unwords [show ty, show v, "=", show e, ";"])
  If i t e ->
    vert [ lay ("if (" ++ show i ++ ")")
         , layCStat t
         , lay "else"
         , layCStat e
         ]

instance Show CExp where
  show = \case
    LitI n -> show n
    LitS s -> show s
    LitB b -> "0x" ++ show b
    LitA a -> "0x" ++ show a
    Ident s -> show s
    Call f args -> unwords [show f,"(",intercalate "," [ show e | e <- args],")"]
    Assign name exp -> unwords [show name, "=", show exp]

brace :: Lay -> Lay
brace x = vert [ lay "{", tab x, lay "}" ]
