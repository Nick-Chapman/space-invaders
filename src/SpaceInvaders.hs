
module SpaceInvaders(main) where

import InstructionSet (printDecodeTable)
import System.Environment (getArgs)
import TraceEmu (traceEmulate,Period(Second,HalfFrame),TraceConf(..))
import qualified Mem (init)
import qualified Rom2k (load)
import qualified GraphicsSDL (main)
import qualified Tst (main)
import qualified SpeedTest (main)
import qualified Static (main)

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
    ModeSDL -> do
      GraphicsSDL.main fps mem
    ModeTst -> do
      Tst.main
    ModeSpeedTest -> do
      SpeedTest.main mem
    ModeStatic -> do
      Static.main

data Mode = ModeShowDecodeTable | ModeTrace | ModeSDL | ModeSpeedTest | ModeTst
  | ModeStatic

data Conf = Conf
  { mode :: Mode
  , traceConf :: TraceConf
  , fps :: Maybe Int
  }

conf0 :: Conf
conf0 = Conf { mode = ModeSDL , traceConf = traceConf0, fps = Nothing } -- half speed default

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
  , stopAfter = Just 10000000 -- 10mil instructions, approx 2400 frames, or 40 emulated seconds
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
  "static":args -> parse args $ conf { mode = ModeStatic }
  "speed-test":args -> parse args $ conf { mode = ModeSpeedTest }
  "tst":args -> parse args $ conf { mode = ModeTst }
  "sdl":args -> parse args $ conf { mode = ModeSDL }
  "decode":args -> parse args $ conf { mode = ModeShowDecodeTable }
  "trace":args -> parse args $ conf { mode = ModeTrace }
  "test1":args -> parse args $ conf { mode = ModeTrace, traceConf = traceConfTest1 }
  "test2":args -> parse args $ conf {mode = ModeTrace, traceConf = traceConfTest2 }
  "-poi":i:args -> parse args $ conf { traceConf = traceConfPOI (read i) }
  "-fps":i:args -> parse args $ conf { fps = Just (read i) }
  "-frame":args -> parse args $ conf { traceConf = (traceConf conf) { period = HalfFrame } }
  "-trace-near-ping":args -> parse args $ conf { traceConf = (traceConf conf) { traceNearPing = True } }
  args ->
    error $ "parseArgs: " <> show args
