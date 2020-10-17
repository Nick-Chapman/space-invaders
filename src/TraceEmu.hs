
module TraceEmu (Conf(..),traceEmulate) where

import Control.Monad (when)
import Data.Bits (testBit)
import Text.Printf (printf)

import Addr (Addr(..))
import Byte (Byte)
import Emulate (Emulation(..),EmuState(..),Ticks(..),emulate,prettyPrefix)
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem,read)

data Conf = Conf { onAfter :: Maybe Int, stopAfter :: Maybe Int }

traceEmulate :: Conf -> Mem -> IO ()
traceEmulate Conf{onAfter,stopAfter} mem = emulate mem >>= loop
  where
    loop :: Emulation -> IO ()
    loop = \case
      EmuStep
        { pre
        , instruction
        , post = post@EmuState{cpu,icount}
        , continue
        } -> do
        when isOn $
          putStrLn (ljust 60 (prettyStep pre instruction) ++ show cpu)
        printWhenNewSecond pre post
        when isStop $ error "STOP"
        continue >>= loop
          where
            isOn = case onAfter of Just i -> (icount > i); Nothing -> False
            isStop = case stopAfter of Just i -> (icount > i); Nothing -> False


ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

prettyStep :: EmuState -> Instruction Byte -> String
prettyStep s i =
  prettyPrefix s $
  unwords [ ljust 10 (prettyInstructionBytes i), show i ]

printWhenNewSecond :: EmuState -> EmuState -> IO ()
printWhenNewSecond s0 s1 = do
  let EmuState{ticks=ticks0} = s0
  let EmuState{ticks=ticks1} = s1
  let f0 = unTicks ticks0 `div` period
  let f1 = unTicks ticks1 `div` period
  let yes = f1 > f0
  when yes $ do
    let EmuState{mem} = s1
    let pixs = onPixels (getDisplayFromMem mem)
    putStrLn $ prettyPrefix s1 $ unwords
      [ printf "SECOND{%d}" f1
      , printf "#onPixels = %d" (length pixs)
      ]
    where
      period = 2000000


_printWhenNewFrame :: EmuState -> EmuState -> IO ()
_printWhenNewFrame s0 s1 = do
  let EmuState{ticks=ticks0} = s0
  let EmuState{ticks=ticks1} = s1
  let f0 = unTicks ticks0 `div` cyclesPerFrame
  let f1 = unTicks ticks1 `div` cyclesPerFrame
  let yes = f1 > f0
  when yes $ do
    let EmuState{mem} = s1
    let pixs = onPixels (getDisplayFromMem mem)
    let _nCyclesLate = unTicks ticks1 `mod` cyclesPerFrame
    let _nFramesSkipped = f1 - f0 - 1
    putStrLn $ prettyPrefix s1 $ unwords
      [ printf "FRAME{%d}" f1
      , printf "#onPixels = %d" (length pixs)
      --, printf "(cycles-late=%d)" _nCyclesLate
      --, printf "(frame-skipped=%d)" _nFramesSkipped
      --, show (f0,f1)
      --, show (ticks0,ticks1)
      ]
    where
      cyclesPerFrame = 33333

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
