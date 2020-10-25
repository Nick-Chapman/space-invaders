
module TraceEmu (TraceConf(..),traceEmulate,Period(..)) where

import Control.Monad (when,forM)
import Data.Bits (testBit)
import Text.Printf (printf)

import Addr (Addr(..))
import Buttons (buttons0)
import Byte (Byte)
import Emulate (EmuState(..),initState,Ticks(..),prettyPrefix,emulate,EmuStep(..))
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem,read)

data TraceConf = TraceConf
  { traceOnAfter :: Maybe Int
  , stopAfter :: Maybe Int
  , period :: Period
  , traceNearPing :: Bool
  }

traceEmulate :: TraceConf -> Mem -> IO ()
traceEmulate TraceConf{traceOnAfter,stopAfter,period,traceNearPing} mem = do
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
        True -> putStrLn "STOP"
        False -> do

          let ping = (ticks >= nextPing)
          let nearPing = ((ticks+50 >= nextPing) || (ticks-50 <= nextPing-cycles))

          when (traceIsOn || (traceNearPing && nearPing)) $
            putStrLn (ljust 60 (prettyStep pre instruction) ++ show post)

          case ping of
            False ->  loop periodCount nextPing post
            True -> do
              printPeriodPixels period periodCount post
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


printPeriodPixels :: Period -> Int -> EmuState -> IO ()
printPeriodPixels period count s@EmuState{mem} = do
  Display{onPixels} <- getDisplayFromMem mem
  putStrLn $ prettyPrefix s $ unwords
    [ printf "%s{%d}" (show period) count
    , printf "#onPixels = %d" (length onPixels)
    ]

data OnPixel = OnPixel { x :: Int, y :: Int }

data Display = Display { onPixels :: [OnPixel] }

getDisplayFromMem :: Mem -> IO Display
getDisplayFromMem mem = do
  let trips =
        [ (x,yByte,a)
        | x :: Int <- [0..223]
        , yByte :: Int <- [0..31]
        , let a = Addr (fromIntegral (0x2400 + x * 32 + yByte))
        ]
  (Display . concat) <$> do
    forM trips $ \(x,yByte,a) -> do
      byte <- Mem.read error mem a
      return [ OnPixel {x,y}
             | yBit :: Int <- [0..7]
             , byte `testBit` yBit
             , let y = 8 * yByte + yBit
             ]
