
module TraceEmu (Conf(..),traceEmulate,Period(..)) where

import Control.Monad (when)
import Data.Bits (testBit)
import Text.Printf (printf)

import Addr (Addr(..))
import Buttons (buttons0)
import Byte (Byte)
import Emulate (Emulation(..),EmuState(..),Ticks(..),emulate,prettyPrefix)
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem,read)

data Conf = Conf
  { traceOnAfter :: Maybe Int
  , stopAfter :: Maybe Int
  , period :: Period
  }

traceEmulate :: Conf -> Mem -> IO ()
traceEmulate Conf{traceOnAfter,stopAfter,period} mem = emulate mem >>= loop
  where
    loop :: Emulation -> IO ()
    loop = \case
      EmuStep
        { pre
        , instruction
        , post = post@EmuState{cpu=_,icount}
        , continue
        } -> do

        when traceIsOn $
          putStrLn (ljust 60 (prettyStep pre instruction) ++ show post)

        case reachNewPeriod period pre post of
          Nothing -> return ()
          Just f1 -> printPeriodPixels period f1 post

        case isStop of
          True -> putStrLn "STOP"
          False -> continue buttons0 >>= loop

          where
            traceIsOn = case traceOnAfter of Just i -> (icount > i); Nothing -> False
            isStop = case stopAfter of Just i -> (icount > i); Nothing -> False


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


reachNewPeriod :: Period -> EmuState -> EmuState -> Maybe Int
reachNewPeriod period s0 s1 = do
  let EmuState{ticks=ticks0} = s0
  let EmuState{ticks=ticks1} = s1
  let cycles = cyclesInPeriod period
  let f0 = unTicks ticks0 `div` cycles
  let f1 = unTicks ticks1 `div` cycles
  let yes = f1 > f0
  if yes then Just f1 else Nothing


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
