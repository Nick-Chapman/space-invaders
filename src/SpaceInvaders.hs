
module SpaceInvaders (main) where

import System.Environment (getArgs)
import System.IO (Handle,withFile,IOMode(WriteMode),stdout)
import TraceEmu (traceEmulate,Period(Second,HalfFrame),TraceConf(..))
import qualified GraphicsSDL as SDL (main,Conf(..))
import qualified SpeedTest (main)
import qualified Static (main)
import qualified Tst (main)

-- | Entry point to the Space Invaders emulation
main :: IO ()
main = do
  putStrLn "*space-invaders*"
  args <- getArgs
  let Conf{mode,traceConf,fpsLimit,scaleFactor,showControls} = parse args conf0
  case mode of
    ModeTest -> do test0; test1 -- ; test2 -- skip test2, too slow
    ModeTest0 -> test0
    ModeTest1 -> test1
    ModeTest2 -> test2
    ModeTrace -> do
      traceEmulate stdout traceConf
    ModeSpeedTest -> do
      SpeedTest.main
    ModeSDL -> do
      SDL.main $ SDL.Conf { scaleFactor, fpsLimit, showControls }
    ModeStatic -> do
      Static.main
  where
    test0 = do
      withTraceFile "test0" $ \h -> do
        Tst.main h
    test1 = do
      withTraceFile "test1" $ \h -> do
        traceEmulate h traceConfTest1
    test2 = do
      withTraceFile "test2" $ \h -> do
        traceEmulate h traceConfTest2

withTraceFile :: String -> (Handle -> IO ()) -> IO ()
withTraceFile tag f = do
  let path :: FilePath = "trace/" ++ tag ++ ".out"
  withFile path WriteMode $ \handle -> do
    putStrLn $ "Writing to file: " <> path
    f handle

data Mode
  = ModeTest -- all the tests
  | ModeTest0 | ModeTest1 | ModeTest2 -- selected tests
  | ModeTrace | ModeSpeedTest | ModeSDL | ModeStatic

data Conf = Conf
  { mode :: Mode
  , traceConf :: TraceConf
  , fpsLimit :: Maybe Int
  , scaleFactor :: Int
  , showControls :: Bool
  }

conf0 :: Conf
conf0 = Conf
  { mode = ModeSDL
  , traceConf = traceConf0
  , fpsLimit = Nothing
  , scaleFactor = 3
  , showControls = False
  }

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
  "test":args -> parse args $ conf { mode = ModeTest }
  "test0":args -> parse args $ conf { mode = ModeTest0 }
  "test1":args -> parse args $ conf { mode = ModeTest1 }
  "test2":args -> parse args $ conf { mode = ModeTest2 }
  "trace":args -> parse args $ conf { mode = ModeTrace }
  "speed-test":args -> parse args $ conf { mode = ModeSpeedTest }
  "sdl":args -> parse args $ conf { mode = ModeSDL }
  "static":args -> parse args $ conf { mode = ModeStatic }
  "-poi":i:args -> parse args $ conf { traceConf = traceConfPOI (read i) }
  "-controls":args -> parse args $ conf { showControls = True }
  "-no-controls":args -> parse args $ conf { showControls = False }
  "-sf":i:args -> parse args $ conf { scaleFactor = read i }
  "-fps":i:args -> parse args $ conf { fpsLimit = Just (read i) }
  "-frame":args -> parse args $ conf { traceConf = (traceConf conf) { period = HalfFrame } }
  "-trace-near-ping":args -> parse args $ conf { traceConf = (traceConf conf) { traceNearPing = True } }
  args ->
    error $ "parseArgs: " <> show args
