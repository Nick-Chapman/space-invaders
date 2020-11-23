
module TraceEmu (TraceConf(..),traceEmulate,Period(..)) where

import Addr (Addr(..))
import Buttons (buttons0)
import Byte (Byte)
import Control.Monad (when)
import Data.Bits (testBit)
import Emulate (EmuState(..),initState,Ticks(..),CB(..),emulate)
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem)
import System.IO (Handle,hPutStrLn)
import Text.Printf (printf)
import qualified Mem (read)
import qualified Rom (loadInvaders)

data TraceConf = TraceConf
  { traceOnAfter :: Maybe Int
  , stopAfter :: Maybe Int
  , period :: Period
  , traceNearPing :: Bool
  }

traceEmulate :: Handle -> TraceConf -> IO ()
traceEmulate handle TraceConf{traceOnAfter,stopAfter,period,traceNearPing} = do
  rom <- Rom.loadInvaders
  state <- initState rom
  loop 1 firstPing state
  where
    firstPing = cycles
    cycles = Ticks (cyclesInPeriod period)

    loop :: Int -> Ticks -> EmuState -> IO ()
    loop periodCount nextPing pre = do

      let EmuState{icount,ticks} = pre

      let traceIsOn = case traceOnAfter of Just i -> (icount >= i); Nothing -> False
      let isStop = case stopAfter of Just i -> (icount >= i); Nothing -> False

      let nearPing = ((ticks+50 >= nextPing) || (ticks-50 <= nextPing-cycles))

      let
        traceI :: EmuState -> Instruction Byte -> IO ()
        traceI s i = do
          when (traceIsOn || (traceNearPing && nearPing)) $
            hPutStrLn handle $ traceLine s i

        cb :: CB
        cb = CB { traceI }

      post <- emulate cb buttons0 pre

      case isStop of
        True -> hPutStrLn handle "STOP"
        False -> do
          let EmuState{ticks} = post
          let ping = (ticks >= nextPing)
          case ping of
            False ->  loop periodCount nextPing post
            True -> do
              hPutStrLn handle $ printPeriodPixels pre period periodCount
              loop (periodCount + 1) (nextPing + cycles) post


traceLine :: EmuState -> Instruction Byte -> String
traceLine s@EmuState{ticks,icount} i = do
  unwords
    [ printf "%8d" icount
    , rjust 11 (show ticks)
    , show s
    , ":"
    , ljust 10 (prettyInstructionBytes i)
    , show i
    ]


data Period = Second | HalfFrame deriving Show

cyclesInPeriod :: Period -> Int
cyclesInPeriod = \case
  Second -> twoMill
  HalfFrame -> twoMill `div` 120
  where twoMill = 2_000_000


printPeriodPixels :: EmuState -> Period -> Int -> String
printPeriodPixels s@EmuState{ticks,icount} period count = do
  let EmuState{mem} = s
  let pixs = onPixels (getDisplayFromMem mem)
  unwords
    [ printf "%8d" icount
    , rjust 11 (show ticks)
    , show s
    , printf "%s{%d}" (show period) count
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
    , let byte = Mem.read mem (Addr (fromIntegral (0x2400 + x * 32 + yByte)))
    , yBit <- [0..7]
    , byte `testBit` yBit
    , let y  = 8 * yByte + yBit
    ]


ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

rjust :: Int -> String -> String
rjust n s = take (max 0 (n - length s)) (repeat ' ') <> s
