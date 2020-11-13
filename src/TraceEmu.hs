
module TraceEmu (TraceConf(..),traceEmulate,Period(..)) where

import Addr (Addr(..))
import Buttons (buttons0)
import Byte (Byte)
import Control.Monad (when)
import Data.Bits (testBit)
import Emulate (EmuState(..),initState,Ticks(..),prettyPrefix,emulate,EmuStep(..))
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem)
import System.IO (Handle,hPutStrLn)
import Text.Printf (printf)
import qualified Mem (read,initInvader)

data TraceConf = TraceConf
  { traceOnAfter :: Maybe Int
  , stopAfter :: Maybe Int
  , period :: Period
  , traceNearPing :: Bool
  }

traceEmulate :: Handle -> TraceConf -> IO ()
traceEmulate handle TraceConf{traceOnAfter,stopAfter,period,traceNearPing} = do
  mem <- Mem.initInvader
  loop 1 firstPing (initState mem)
  where
    firstPing = cycles
    cycles = Ticks (cyclesInPeriod period)

    loop :: Int -> Ticks -> EmuState -> IO ()
    loop periodCount nextPing pre = do

      EmuStep{instruction,post} <- emulate buttons0 pre
      let EmuState{icount,ticks} = post

      let traceIsOn = case traceOnAfter of Just i -> (icount > i); Nothing -> False
      let isStop = case stopAfter of Just i -> (icount > i+1); Nothing -> False

      case isStop of
        True -> hPutStrLn handle "STOP"
        False -> do

          let ping = (ticks >= nextPing)
          let nearPing = ((ticks+50 >= nextPing) || (ticks-50 <= nextPing-cycles))

          when (traceIsOn || (traceNearPing && nearPing)) $
            hPutStrLn handle (ljust 60 (prettyStep pre instruction) ++ show post)

          case ping of
            False ->  loop periodCount nextPing post
            True -> do
              printPeriodPixels handle period periodCount post
              loop (periodCount + 1) (nextPing + cycles) post


ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

prettyStep :: EmuState -> Instruction Byte -> String
prettyStep s i =
  prettyPrefix s $
  unwords [ ljust 10 (prettyInstructionBytes i), show i ]


data Period = Second | HalfFrame deriving Show

cyclesInPeriod :: Period -> Int
cyclesInPeriod = \case
  Second -> twoMill
  HalfFrame -> twoMill `div` 120
  where twoMill = 2_000_000


printPeriodPixels :: Handle -> Period -> Int -> EmuState -> IO ()
printPeriodPixels handle period count s = do
  let EmuState{mem} = s
  let pixs = onPixels (getDisplayFromMem mem)
  hPutStrLn handle $ prettyPrefix s $ unwords
    [ printf "%s{%d}" (show period) count
    , printf "#onPixels = %d" (length pixs)
    ]

data OnPixel = OnPixel { x :: Int, y :: Int }

data Display = Display { onPixels :: [OnPixel] }

getDisplayFromMem :: Mem -> Display
getDisplayFromMem mem = do
  Display
    [ OnPixel {x, y}
    | x :: Int <- [0..223]
    , yByte <- [0..31]
    , let byte = Mem.read error mem (Addr (fromIntegral (0x2400 + x * 32 + yByte)))
    , yBit <- [0..7]
    , byte `testBit` yBit
    , let y  = 8 * yByte + yBit
    ]
