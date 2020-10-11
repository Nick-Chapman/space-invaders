
module SpaceInvaders(main) where

import Emulate (emulate)
import InstructionSet (printDecodeTable)
import System.Environment (getArgs)
import qualified Mem (init)
import qualified Rom2k (load)

-- | Entry point to the Space Invaders emulation
main :: IO ()
main = do
  putStrLn "*space-invaders*"

  e <- Rom2k.load "roms/invaders.e"
  f <- Rom2k.load "roms/invaders.f"
  g <- Rom2k.load "roms/invaders.g"
  h <- Rom2k.load "roms/invaders.h"

  let mem = Mem.init (e,f,g,h)
  args <- getArgs
  let Conf{mode} = parse args conf0
  case mode of
    ModeShowDecodeTable ->
      printDecodeTable
    ModeTrace ->
      emulate True mem
    ModeNoTrace ->
      emulate False mem



data Mode = ModeShowDecodeTable | ModeTrace | ModeNoTrace

data Conf = Conf { mode :: Mode }

conf0 :: Conf
conf0 = Conf { mode = ModeTrace }

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "table":args -> parse args $ Conf { mode = ModeShowDecodeTable }
  "trace":args -> parse args $ Conf { mode = ModeTrace }
  "notrace":args -> parse args $ Conf { mode = ModeNoTrace }

  args -> error $ "parseArgs: " <> show args
