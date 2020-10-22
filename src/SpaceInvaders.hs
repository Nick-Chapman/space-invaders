
module SpaceInvaders(main) where

import InstructionSet (printDecodeTable)
import System.Environment (getArgs)
import TraceEmu (traceEmulate,Period(Second,HalfFrame),TraceConf(..))
import qualified Gloss (run)
import qualified Mem (init)
import qualified Rom2k (load)
import qualified GraphicsSDL (main)

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
    ModeSDL -> do
      GraphicsSDL.main mem

data Mode = ModeShowDecodeTable | ModeTrace | ModeGloss | ModeSDL

data Conf = Conf
  { mode :: Mode
  , traceConf :: TraceConf
  , fps :: Int
  }

conf0 :: Conf
conf0 = Conf { mode = ModeSDL , traceConf = traceConf0, fps = 30 } -- half speed default

traceConf0 :: TraceConf
traceConf0 = TraceConf
  { traceOnAfter = Nothing -- dont trace instruction
  , stopAfter = Nothing
  , period = Second
  , traceNearPing = False
  }

traceConfTest1 :: TraceConf
traceConfTest1 = traceConf0
  { traceOnAfter = Just 0 -- trace every instruction from the start
  , stopAfter = Just 50000 --50k instructions
  }

traceConfTest2 :: TraceConf
traceConfTest2 = traceConf0
  { traceOnAfter = Nothing -- dont trace every instruction
  , stopAfter = Just 1000000 -- 1mil instructions
  , period = HalfFrame
  }

traceConfPOI :: Int -> TraceConf
traceConfPOI i = traceConf0
  { traceOnAfter = Just (i-5) -- trace instruction from just before the POI...
  , stopAfter = Just (i+5) -- until just after
  , period = HalfFrame
  }

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "sdl":args -> parse args $ conf { mode = ModeSDL }
  "gloss":args -> parse args $ conf { mode = ModeGloss }
  "decode":args -> parse args $ conf { mode = ModeShowDecodeTable }
  "trace":args -> parse args $ conf { mode = ModeTrace }
  "test1":args -> parse args $ conf { traceConf = traceConfTest1 }
  "test2":args -> parse args $ conf { traceConf = traceConfTest2 }
  "-poi":i:args -> parse args $ conf { traceConf = traceConfPOI (read i) }
  "-fps":i:args -> parse args $ conf { fps = read i }
  "-frame":args -> parse args $ conf { traceConf = (traceConf conf) { period = HalfFrame } }
  "-trace-near-ping":args -> parse args $ conf { traceConf = (traceConf conf) { traceNearPing = True } }
  args ->
    error $ "parseArgs: " <> show args
