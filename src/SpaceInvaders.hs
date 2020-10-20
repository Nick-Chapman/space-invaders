
module SpaceInvaders(main) where

import InstructionSet (printDecodeTable)
import System.Environment (getArgs)
import TraceEmu (traceEmulate,Period(Second,HalfFrame))
import qualified TraceEmu
import qualified Gloss (run)
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
  let Conf{mode,traceConf,fps} = parse args conf0
  case mode of
    ModeShowDecodeTable ->
      printDecodeTable
    ModeTrace -> do
      traceEmulate traceConf mem
    ModeGloss -> do
      Gloss.run fps mem

data Mode = ModeShowDecodeTable | ModeTrace | ModeGloss

data Conf = Conf
  { mode :: Mode
  , traceConf :: TraceEmu.Conf
  , fps :: Int
  }

conf0 :: Conf
conf0 = Conf { mode = ModeTrace , traceConf = traceConf0, fps = 30 } -- half speed default

traceConf0 :: TraceEmu.Conf
traceConf0 = TraceEmu.Conf
  { traceOnAfter = Nothing -- dont trace instruction
  , stopAfter = Nothing
  , period = Second
  }

traceConfTest1 :: TraceEmu.Conf
traceConfTest1 = TraceEmu.Conf
  { traceOnAfter = Just 0 -- trace every instruction from the start
  , stopAfter = Just 50000 --50k instructions
  , period = Second
  }

traceConfTest2 :: TraceEmu.Conf
traceConfTest2 = TraceEmu.Conf
  { traceOnAfter = Nothing -- dont trace every instruction
  , stopAfter = Just 1000000 -- 1mil instructions
  , period = HalfFrame
  }

traceConfPOI :: Int -> TraceEmu.Conf
traceConfPOI i = TraceEmu.Conf
  { traceOnAfter = Just (i-5) -- trace instruction from just before the POI...
  , stopAfter = Just (i+5) -- until just after
  , period = HalfFrame
  }

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "gloss":args -> parse args $ conf { mode = ModeGloss }
  "decode":args -> parse args $ conf { mode = ModeShowDecodeTable }
  "test1":args -> parse args $ conf { traceConf = traceConfTest1 }
  "test2":args -> parse args $ conf { traceConf = traceConfTest2 }
  "-poi":i:args -> parse args $ conf { traceConf = traceConfPOI (read i) }
  "-fps":i:args -> parse args $ conf { fps = read i }
  args ->
    error $ "parseArgs: " <> show args
