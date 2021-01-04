
module GenerateC (main) where

import Addr (Addr)
import Compile (compileAt)
import Data.List (intercalate)
import Residual --(Program)
import Rom (Rom)
import qualified Rom (loadInvaders)

main :: IO ()
main = do
  putStrLn "*GenerateC*"
  rom <- Rom.loadInvaders
  let cfile = convertRom rom
  let fp = "c/program.c"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show cfile)

convertRom :: Rom -> CFile
convertRom rom = cfile
  where
    cfile = CFile $ [include1,include2] ++ defs
    defs = [ FunDef $ convertProgramForAddress addr program
           | addr <- [0..3] -- 0x1FFF]
           , let program = compileAt (\_ -> False) rom addr
           ]
    include1 = Include "<stdio.h>"
    include2 = Include "\"machine.h\""

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
  S_AtRef a next ->
    --Comment ("#" ++ show a) : convertProgram next
    Expression (Call (CName "at") [LitS $ show a]) : convertProgram next

  S_MarkReturnAddress{} -> undefined

  S_TraceInstruction _cpu i _ next ->
    --Comment ("#instruction: " ++ show i) : convertProgram next
    Expression (Call (CName "instruction") [LitS $ show i]) : convertProgram next

  S_Advance n next ->
    Expression (Call (CName "advance") [LitI n]) : convertProgram next

  S_Jump e ->
    [Return (Call (CName "jump") [convert16 e])]

  S_If{} -> undefined
  S_AssignReg{} -> undefined
  S_AssignFlag{} -> undefined
  S_AssignShifterReg{} -> undefined
  S_MemWrite{} -> undefined
  S_Let17{} -> undefined
  S_Let16{} -> undefined
  S_Let8{} -> undefined
  S_SoundControl{} -> undefined
  S_EnableInterrupts{} -> undefined
  S_DisableInterrupts{} -> undefined
  S_UnknownOutput{} -> undefined

convert16 :: Exp16 -> CExp
convert16 = \case
  E16_Lit a -> Ident (nameOfAddr a)
  x -> error $ "convert16: " ++ show x

data CFile = CFile [CTop]

data CTop
  = Include String
  | FunDef CFunDef

data CFunDef = CFunDef
  { typ :: CType
  , name :: CName
  , body :: CStat
  }

data CType = CType String
data CName = CName String

data CStat
  = Printf String
  | Block [CStat]
  | Return CExp
  | Expression CExp
  | Comment String

data CExp
  = LitI Int
  | LitS String
  | Ident CName
  | Call CName [CExp]

instance Show CFile where
  show (CFile tops) = unlines (map show tops)

instance Show CTop where
  show = \case
    Include what -> unwords ["#include",what]
    FunDef def -> show def

instance Show CFunDef where
  show CFunDef{typ,name,body} =
    show $ vert [ lay (unwords [show typ, show name, "()"])
                , layCStat body]

instance Show CType where show (CType s) = s

instance Show CName where show (CName s) = s

instance Show CStat where show = show . layCStat

layCStat :: CStat -> Lay
layCStat = \case
  Printf mes -> lay (unwords ["printf","(",dq++mes++dq,")",";"])
    where dq = "\""
  Block xs ->
    brace (tab (vert [ layCStat x | x <- xs ]))

  Return e -> lay ("return " ++ show e ++ ";")
  Expression e -> lay (show e ++ ";")
  Comment s -> lay ("// " ++ s)

instance Show CExp where
  show = \case
    LitI n -> show n
    LitS s -> show s
    Ident s -> show s
    Call f args ->
      unwords [show f,"(",intercalate "," [ show e | e <- args],")"]

brace :: Lay -> Lay
brace x = vert [ lay "{", tab x, lay "}" ]
