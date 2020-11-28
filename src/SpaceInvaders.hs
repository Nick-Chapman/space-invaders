
module SpaceInvaders (main) where

import System.Environment (getArgs)
import System.IO (Handle,withFile,IOMode(WriteMode),stdout)
import TraceEmu (traceEmulate,TraceConf(..))
import qualified GraphicsSDL as SDL (main,Conf(..))
import qualified SpeedTest (main)
import qualified Static (main)
import qualified Tst (main)

-- | Entry point to the Space Invaders emulation
main :: IO ()
main = do
  putStrLn "*space-invaders*"
  args <- getArgs
  let Conf{mode,fpsLimit,scaleFactor,showControls} = parse args conf0
  case mode of
    ModeTest -> do test0; test1 -- ; test2 -- skip test2, too slow
    ModeTest0 -> test0
    ModeTest1 -> test1
    ModeTest2 -> test2
    ModeTrace -> do
      traceEmulate stdout traceConf
    ModeSpeedTest -> do
      SpeedTest.main
    ModeStatic -> do
      Static.main
    ModePlay -> do
      SDL.main $ SDL.Conf { scaleFactor, fpsLimit, showControls }
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
  | ModeTrace | ModeSpeedTest | ModeStatic
  | ModePlay

data Conf = Conf
  { mode :: Mode
  , fpsLimit :: Maybe Int
  , scaleFactor :: Int
  , showControls :: Bool
  }

conf0 :: Conf
conf0 = Conf
  { mode = ModePlay
  , fpsLimit = Nothing
  , scaleFactor = 3
  , showControls = False
  }

traceConf :: TraceConf
traceConf = TraceConf
  { stopAfter = Nothing
  , iPeriod = 500_000 -- ~ 2.25 emulated seconds.
  , showPixs = True
  }

traceConfTest1 :: TraceConf
traceConfTest1 = TraceConf
  { stopAfter = Just 50000 -- ~1/5 emulated second. just long enough for interrupts to become enabled
  , iPeriod = 1
  , showPixs = False
  }

traceConfTest2 :: TraceConf
traceConfTest2 = TraceConf
  { stopAfter = Just 10_000_000 -- 10mil instructions, aprox 44 emulated seconds
  , iPeriod = 10_000
  , showPixs = True
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
  "static":args -> parse args $ conf { mode = ModeStatic }
  "-controls":args -> parse args $ conf { showControls = True }
  "-no-controls":args -> parse args $ conf { showControls = False }
  "-sf":i:args -> parse args $ conf { scaleFactor = read i }
  "-fps":i:args -> parse args $ conf { fpsLimit = Just (read i) }
  args ->
    error $ "parseArgs: " <> show args
