
module SpaceInvaders(main) where

import InstructionSet (printDecodeTable)
import System.Environment (getArgs)
import TraceEmu (traceEmulate)
import qualified TraceEmu
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
  let Conf{mode,traceConf} = parse args conf0
  case mode of
    ModeShowDecodeTable ->
      printDecodeTable
    ModeTrace -> do
      traceEmulate traceConf mem

data Mode = ModeShowDecodeTable | ModeTrace

data Conf = Conf
  { mode :: Mode
  , traceConf :: TraceEmu.Conf
  }

conf0 :: Conf
conf0 = Conf { mode = ModeTrace , traceConf = traceConf0 }

traceConf0 :: TraceEmu.Conf
traceConf0 = TraceEmu.Conf { onAfter = Nothing, stopAfter = Nothing }

traceConfTest1 :: TraceEmu.Conf
traceConfTest1 = TraceEmu.Conf { onAfter = Just 0, stopAfter = Just 50000 }

traceConfPOI :: Int -> TraceEmu.Conf
traceConfPOI i = TraceEmu.Conf { onAfter = Just (i-5), stopAfter = Just (i+5) }

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "decode":args -> parse args $ conf { mode = ModeShowDecodeTable }
  "test1":args -> parse args $ conf { traceConf = traceConfTest1 }
  "-poi":i:args -> parse args $ conf { traceConf = traceConfPOI (read i) }
  args ->
    error $ "parseArgs: " <> show args
