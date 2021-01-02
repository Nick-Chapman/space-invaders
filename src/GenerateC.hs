
module GenerateC (main) where

import Compile (compileAt)
import Residual (Program)
import qualified Rom (loadInvaders)

main :: IO ()
main = do
  putStrLn "*GenerateC*"
  roms <- Rom.loadInvaders
  let p0 :: Program = compileAt (\_ -> False) roms 0
  let cfile :: CFile = convert p0
  let fp = "c/program.c"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show cfile)

convert :: Program -> CFile
convert _ = cfile
  where
    cfile = CFile [include,FunDef main]
    include = Include "<stdio.h>"
    main = CFunDef { typ = int, name = CName "main", body = hello }
    int = CTyp "int"
    hello = Printf "hello, world\\n"

data CFile = CFile  [CTop]

data CTop
  = Include String
  | FunDef CFunDef

data CFunDef = CFunDef
  { typ :: CTyp
  , name :: CName
  , body :: CStat
  }

data CTyp = CTyp String
data CName = CName String

data CStat
  = Printf String

instance Show CFile where
  show (CFile tops) = unlines (map show tops)

instance Show CTop where
  show = \case
    Include what -> unwords ["#include",what]
    FunDef def -> show def

instance Show CFunDef where
  show CFunDef{typ,name,body} =
    unwords [show typ, show name, "()", "{", show body, "}"]

instance Show CTyp where show (CTyp s) = s

instance Show CName where show (CName s) = s

instance Show CStat where
  show = \case
    Printf mes -> unwords ["printf","(",dq++mes++dq,")",";"]
      where dq = "\""
