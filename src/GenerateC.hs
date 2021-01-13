
module GenerateC (main) where

import Prelude hiding (init)

import Addr (Addr)
import Byte (Byte)
import Compile (compileOp,compileAt,compileInstruction)
import Cpu (Reg(..),Flag(..))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromJust)
import HiLo (HiLo(..))
import InstructionSet (Op(Op0,Op1),Op0(RST),Op1(IN,OUT),Instruction(..),decode)
import Residual (Exp1(..),Exp8(..),Exp16(..),Exp17(..),Program(..),AVar,Lay,vert,lay,tab)
import Rom (Rom)
import Static (oneStepReach,searchReach,startPoints,returnAddresses)
import qualified Data.Map.Strict as Map
import qualified Rom (loadInvaders,lookup,size)
import qualified Shifter (Reg)
import qualified Data.Set as Set

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
    mem =
      ArrDef (CArrDef
               { typ=u8t
               , name=CName "mem"
               , size = Ident (CName "MEM_SIZE")
               , init = [ LitB $ fromJust $ Rom.lookup rom a
                        | a <- take 0x2000 [0..] ]
               })

  let
    skippedOps =
      [
        -- We skip generating programs for op-codes IN/OUT, because we can only cope
        -- when the port to which the IN/OUT refers is known at generation time
        -- This is not the case for general IN/OUT opcodes, as the port is an immeidate byte.

        -- Instead we generate a family of programs for IN/OUT instructions, for the small
        -- range of ports which are expected. IN: 0..3, OUT: 0..6
        Op1 IN, Op1 OUT

        -- Reset instruction do a jumpDirect to specific addresses. For some of these
        -- address we have no generated code. We could regard these address as additional
        -- start points. But ntead we just skip the gernation of programs for certain
        -- reset instructions. This is ok because we know these instructions are never
        -- used by the space-invaders program,
      , Op0 (RST 3), Op0 (RST 5), Op0 (RST 6), Op0 (RST 7)
      ]
    op_defs =
      [ FunDef $ CFunDef
        { typ = CType "Control"
        , name = CName ("op_" ++ show byte)
        , body = Block (convertProgram nameOfSlowDef program)
        }
      | byte :: Byte <- [0..0xFF]
      , let op = decode byte
      , op `notElem` skippedOps
      , let program = compileOp rom op
      ]
    ops_array =
      ArrDef (CArrDef
               { typ  = CType "Func"
               , name = CName "ops_array"
               , size = LitI 256
               , init = [ if decode b `notElem` skippedOps
                          then Ident (nameOfOpDef b)
                          else LitI 0
                        | b <- take 256 [0..] ]
               })
  let
    max_output = 6
    output_defs =
      [ FunDef $ CFunDef
        { typ = CType "Control"
        , name = CName ("output_" ++ show byte)
        , body = Block (convertProgram nameOfSlowDef program)
        }
      | byte :: Byte <- [0..max_output]
      , let i = Ins1 OUT (E8_Lit byte)
      , let program = compileInstruction rom i
      ]
    output_array =
      ArrDef (CArrDef
               { typ  = CType "Func"
               , name = CName "output_instruction_array"
               , size = LitB (max_output + 1)
               , init = [Ident (CName ("output_" ++ show n))
                        | n <- [0..max_output]
                        ]
               })

  let
    max_input = 4
    input_defs =
      [ FunDef $ CFunDef
        { typ = CType "Control"
        , name = CName ("input_" ++ show byte)
        , body = Block (convertProgram nameOfSlowDef program)
        }
      | byte :: Byte <- [0..max_input]
      , let i = Ins1 IN (E8_Lit byte)
      , let program = compileInstruction rom i
      ]
    input_array =
      ArrDef (CArrDef
               { typ  = CType "Func"
               , name = CName "input_instruction_array"
               , size = LitB (max_input + 1)
               , init = [Ident (CName ("input_" ++ show n))
                        | n <- [0..max_input]
                        ]
               })

  slowProgs <- slowProgramsOfRom rom
  let
    slow_forwards =
      [FunDec $ CFunDec
        { typ = CType "Control"
        , name = nameOfSlowDef addr
        }
      | (addr,_) <- slowProgs
      ]
    slow_defs =
      [ FunDef $ CFunDef
        { typ = CType "Control"
        , name = nameOfSlowDef addr
        , body = Block (convertProgram nameOfSlowDef program)
        }
      | (addr,program) <- slowProgs
      ]
    slowReachSet = Set.fromList (map fst slowProgs)
    slow_progs_array =
      ArrDef (CArrDef
               { typ  = CType "Func"
               , name = CName "slow_progs_array"
               , size = Ident (CName "ROM_SIZE")
               , init = [ if a `elem` slowReachSet
                          then Ident (nameOfSlowDef a)
                          else LitI 0
                        | a <- take 0x2000 [0..] ]
               })

  fastProgs <- fastProgramsOfRom rom
  let
    fast_forwards =
      [FunDec $ CFunDec
        { typ = CType "Control"
        , name = nameOfFastDef addr
        }
      | (addr,_) <- fastProgs
      ]
    fast_defs =
      [ FunDef $ CFunDef
        { typ = CType "Control"
        , name = nameOfFastDef addr
        , body = Block (convertProgram nameOfFastDef program)
        }
      | (addr,program) <- fastProgs
      ]
    fastReachSet = Set.fromList (map fst fastProgs)
    fast_progs_array =
      ArrDef (CArrDef
               { typ  = CType "Func"
               , name = CName "fast_progs_array"
               , size = Ident (CName "ROM_SIZE")
               , init = [ if a `elem` fastReachSet
                          then Ident (nameOfFastDef a)
                          else LitI 0
                        | a <- take 0x2000 [0..] ]
               })
  let defs =
        [ Include "\"machine.c\"" ]
        ++ [mem]
        ++ slow_forwards
        ++ fast_forwards
        ++ op_defs ++ [ops_array]
        ++ output_defs ++ [output_array]
        ++ input_defs ++ [input_array]
        ++ slow_defs ++ [slow_progs_array]
        ++ fast_defs ++ [fast_progs_array]

  return $ CFile defs


slowProgramsOfRom :: Rom -> IO [(Addr,Program)]
slowProgramsOfRom rom = do
  let
    programsForEveryAddress =
      [ (addr,program)
      | addr <- [0.. Rom.size rom - 1]
      , let program = compileAt (\_ -> False) rom addr ]

  let step :: Addr -> [Addr]
      step a = Map.findWithDefault (error $ "step: " <> show a) a stepMap
        where
          stepMap :: Map Addr [Addr]
          stepMap = Map.fromList [ (a, oneStepReach p) | (a,p) <- programsForEveryAddress ]

  reachSet <- searchReach step startPoints
  let slowProgs = [ (a,p) | (a,p) <- programsForEveryAddress, a `elem` reachSet ]
  pure slowProgs

fastProgramsOfRom :: Rom -> IO [(Addr,Program)]
fastProgramsOfRom rom = do
  let
    programsForEveryAddress =
      [ (addr,program)
      | addr <- [0.. Rom.size rom - 1]
      , let program = compileAt (\_ -> False) rom addr ]

    step :: Addr -> [Addr]
    --step a = Map.findWithDefault (error $ "step: " <> show a) a stepMap
    step a = Map.findWithDefault [] a stepMap
        where
          stepMap :: Map Addr [Addr]
          stepMap = Map.fromList [ (a, oneStepReach p) | (a,p) <- programsForEveryAddress ]

  reachSet <- searchReach step startPoints

  let
    joinPoints = [ b | (b,as) <- collate backward, length as > 1 ]
      where
        backward = [ (b,a) | a <- Set.toList reachSet, b <- step a ]

        collate :: Ord a => [(a,b)] -> [(a,[b])]
        collate pairs = Map.toList $ Map.fromListWith (++) [ (a,[b]) | (a,b) <- pairs ]

    returnPoints =
      [ r
      | (a,p) <- programsForEveryAddress
      , a `elem` reachSet
      , r <- returnAddresses p
      ]

    inlinedSharingJoins =
      [ (addr,program)
      | addr <- Set.toList labels
      , let program = compileAt (`notElem` labels) rom addr ]
      where
        labels = Set.fromList (startPoints ++ returnPoints ++ joinPoints)

  return inlinedSharingJoins


data PatMarker = PatMarker
instance Show PatMarker where show PatMarker = "%02X"

convertI :: Instruction a -> CExp
convertI i = LitS $ show (fmap (const PatMarker) i)

convertProgram :: (Addr -> CName) -> Program  -> [CStat]
convertProgram nameOfDef = convert
  where
    convert = \case
      S_AtRef a next -> Comment ("#at: " ++ show a) : convert next
      S_MarkReturnAddress a next -> Comment ("#mark-return: " ++ show a) : convert next
      S_TraceInstruction _cpu i pcAfterDecode next ->
        case i of
          Ins0 _ ->
            Expression (call "instruction0" [convertI i, convert16 pcAfterDecode]) : convert next
          Ins1 _ b1 ->
            Expression (call "instruction1" [convertI i, convert8 b1, convert16 pcAfterDecode]) : convert next
          Ins2 _ b1 b2 ->
            Expression (call "instruction2" [convertI i, convert8 b2, convert8 b1, convert16 pcAfterDecode]) : convert next

      S_Advance n next -> Expression (call "advance" [LitI n]) : convert next
      S_Jump a -> [Return $ convertJump a]
      S_If i t e -> [If (convert1 i) (Block (convert t)) (Block (convert e))]
      S_AssignReg reg exp next -> Expression (Assign (convertReg reg) (convert8 exp)) : convert next
      S_AssignFlag flag exp next -> Expression (Assign (convertFlag flag) (convert1 exp)) : convert next
      S_AssignShifterReg reg exp next ->
        Expression (Assign (convertShifterReg reg) (convert8 exp)) : convert next
      S_MemWrite i e next -> Expression (call "mem_write" [convert16 i, convert8 e]) : convert next
      S_Let17 v e next -> Declare u17t (convertVar v) (convert17 e) : convert next
      S_Let16 v e next -> Declare u16t (convertVar v) (convert16 e) : convert next
      S_Let8 v e next -> Declare u8t (convertVar v) (convert8 e) : convert next
      S_SoundControl sound bool next ->
        Expression (call "sound_control" [LitS $ show sound, convert1 bool]) : convert next
      S_EnableInterrupts next -> Expression (call "enable_interrupts" []) : convert next
      S_DisableInterrupts next -> Expression (call "disable_interrupts" []) : convert next
      S_UnknownOutput port byte next ->
        Expression (call "unknown_output" [LitI (fromIntegral port), convert8 byte]) : convert next

    convertJump :: Exp16 -> CExp
    convertJump = \case
          E16_Lit a ->
            call "jumpDirect" [LitA a, Ident $ nameOfDef a]
          e ->
            call "jump16" [convert16 e]

nameOfSlowDef :: Addr -> CName
nameOfSlowDef a = CName ("slow_" ++ show a)

nameOfFastDef :: Addr -> CName
nameOfFastDef a = CName ("fast_" ++ show a)

nameOfOpDef :: Byte -> CName
nameOfOpDef b = CName ("op_" ++ show b)

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
  E1_True -> Ident (CName "true")
  E1_False -> Ident (CName "false")
  E1_Flip c -> UnOp "!" (convert1 c)
  E1_IsZero b -> e1_is_zero (convert8 b)
  E1_TestBit b i -> e1_test_bit (convert8 b) (LitI i)
  E1_HiBitOf17 x -> e1_hi_bit_of_17 (convert17 x)
  E1_IsParity b -> call "e1_parity" [convert8 b]
  E1_OrBit c1 c2 -> BinOp "||" (convert1 c1) (convert1 c2)
  E1_AndBit c1 c2 -> BinOp "&&" (convert1 c1) (convert1 c2)
  E1_Button but -> call "e1_is_pressed" [Ident $ CName $ show but]
  where
    e1_is_zero = BinOp "==" (LitI 0)
    e1_test_bit x y = BinOp "&" (BinOp ">>" x y) (LitB 1)
    e1_hi_bit_of_17 x = BinOp "&" (BinOp ">>" x (LitI 16)) (LitB 1)

convert8 :: Exp8 -> CExp
convert8 = \case
  E8_Lit b -> LitB b
  E8_Reg reg -> Ident (convertReg reg)
  E8_ShifterReg reg -> Ident (convertShifterReg reg)
  E8_Hi a -> e8_hi (convert16 a)
  E8_Lo a -> e8_lo (convert16 a)
  E8_ReadMem a -> call "e8_read_mem" [convert16 a]
  E8_UpdateBit e i p -> call "e8_update_bit" [convert8 e, LitI i, convert1 p]
  E8_Complement e -> CastOp u8t (UnOp "~" (convert8 e))
  E8_AndB e1 e2 -> BinOp "&" (convert8 e1) (convert8 e2)
  E8_OrB e1 e2 -> BinOp "|" (convert8 e1) (convert8 e2)
  E8_XorB e1 e2 -> BinOp "^" (convert8 e1) (convert8 e2)
  E8_ShiftRight e1 e2 -> BinOp ">>" (convert8 e1) (convert8 e2)
  E8_ShiftLeft e1 e2 -> BinOp "<<" (convert8 e1) (convert8 e2)
  E8_Ite i t e -> IteOp (convert1 i) (convert8 t) (convert8 e)
  E8_Var v -> Ident (convertVar v)
  E8_UnknownInput e -> call "e8_unknown_input" [LitI (fromIntegral e)]
  where
    e8_hi x = BinOp ">>" x (LitI 8)
    e8_lo x = BinOp "&" x (LitB 0xFF)

convert16 :: Exp16 -> CExp
convert16 = \case
  E16_HiLo HiLo{hi,lo} -> e16_hi_lo (convert8 hi) (convert8 lo)
  E16_OffsetAdr n e -> BinOp "+" (convert16 e) (LitI n)
  E16_Var v -> Ident (convertVar v)
  E16_AddWithCarry cin e1 e2 -> BinOp "+" (BinOp "+" (convert8 e1) (convert8 e2)) (convert1 cin)
  E16_DropHiBitOf17 e -> BinOp "&" (convert17 e) (LitA 0xFFFF)
  E16_Lit x -> LitA x
  where
    e16_hi_lo x y = BinOp "|" (BinOp "<<" x (LitI 8)) y


convert17 :: Exp17 -> CExp
convert17 = \case
  E17_Var v -> Ident (convertVar v)
  E17_Add a1 a2 -> BinOp "+" (convert16 a1) (convert16 a2)

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
  | UnOp String CExp
  | BinOp String CExp CExp
  | IteOp CExp CExp CExp
  | CastOp CType CExp

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
    UnOp name exp -> unwords ["(",name,show exp,")"]
    BinOp name e1 e2 -> unwords ["(",show e1,name,show e2,")"]
    IteOp e1 e2 e3 -> unwords ["(",show e1,"?",show e2,":",show e3,")"]
    CastOp ty exp -> unwords ["(","(",show ty,")",show exp,")"]

brace :: Lay -> Lay
brace x = vert [ lay "{", tab x, lay "}" ]
