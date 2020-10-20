
module TraceEmu (TraceConf(..),traceEmulate,Period(..)) where

import Control.Monad (when)
import Data.Bits (testBit)
import Text.Printf (printf)

import Addr (Addr(..))
import Buttons (buttons0)
import Byte (Byte)
import Emulate (Emulation(..),EmuState(..),Ticks(..),emulate,prettyPrefix)
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem,read)

data TraceConf = TraceConf
  { traceOnAfter :: Maybe Int
  , stopAfter :: Maybe Int
  , period :: Period
  , traceNearPing :: Bool
  }

traceEmulate :: TraceConf -> Mem -> IO ()
traceEmulate TraceConf{traceOnAfter,stopAfter,period,traceNearPing} mem =
  emulate mem >>= loop 1 firstPing
  where
    firstPing = cycles
    cycles = Ticks (cyclesInPeriod period)

    loop :: Int -> Ticks -> Emulation -> IO ()
    loop periodCount nextPing = \case
      EmuStep
        { pre
        , instruction
        , post = post@EmuState{cpu=_,icount,ticks}
        , continue
        } -> do
        case isStop of
          True -> putStrLn "STOP"
          False -> do

            let ping = (ticks >= nextPing)

            let nearPing = ((ticks+50 >= nextPing) || (ticks-50 <= nextPing-cycles))

            when (traceIsOn || (traceNearPing && nearPing)) $
              putStrLn (ljust 60 (prettyStep pre instruction) ++ show post)

            s' <- continue buttons0

            case ping of
              False ->  loop periodCount nextPing s'
              True -> do
                printPeriodPixels period periodCount post
                loop (periodCount + 1) (nextPing + cycles) s'

          where
            traceIsOn = case traceOnAfter of Just i -> (icount > i); Nothing -> False
            isStop = case stopAfter of Just i -> (icount > i+1); Nothing -> False


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


printPeriodPixels :: Period -> Int -> EmuState -> IO ()
printPeriodPixels period count s = do
  let EmuState{mem} = s
  let pixs = onPixels (getDisplayFromMem mem)
  putStrLn $ prettyPrefix s $ unwords
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
