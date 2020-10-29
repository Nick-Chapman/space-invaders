
module SpeedTest (main) where

import Addr (Addr(..))
import Buttons (buttons0)
import Data.Bits (testBit)
import Emulate (EmuState(..),EmuStep(..),Ticks(..),initState,emulate)
import GHC.Int (Int64)
import Mem (Mem,read)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import Text.Printf (printf)

data State = State
  { emuSeconds :: Int64
  , durationNanos :: Int64
  , emuState :: EmuState
  }
  deriving Show

main :: Mem -> IO ()
main mem = do
  (es1,nanos1) <- measureOneEmulatedSecond (initState mem)
  loop State { emuSeconds = 1, durationNanos = nanos1, emuState = es1 }
  where
    loop :: State -> IO ()
    loop s = do
      printStatLine s
      (emuState,nanos) <- measureOneEmulatedSecond (emuState s)
      loop State
        { emuSeconds = emuSeconds s + 1
        , durationNanos = durationNanos s + nanos
        , emuState
        }

printStatLine :: State -> IO ()
printStatLine State{emuSeconds,durationNanos}  =
  putStrLn line
  where
    line = printf "%3d : fps = %.02g, speedup = x%.02g" emuSeconds fps speedup
    actualSeconds :: Double = fromIntegral durationNanos / fromIntegral gig
    fps :: Double = fromIntegral (emuSeconds * 60) / actualSeconds
    speedup :: Double = fps / 60

measureOneEmulatedSecond :: EmuState -> IO (EmuState, Int64)
measureOneEmulatedSecond es = do
  before <- getTime Monotonic
  es' <- emulateForOneEmulatedSecond es
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let nanos = gig * sec + nsec
  return (es',nanos)

gig :: Int64
gig = 1_000_000_000

emulateForOneEmulatedSecond :: EmuState -> IO EmuState
emulateForOneEmulatedSecond es0 = loop es0
  where

    goal :: Ticks
    goal = do
      let EmuState{ticks=t0} = es0
      Ticks (twoMill * (1 + unTicks t0 `div` twoMill))
        where
          twoMill = 2_000_000

    loop :: EmuState -> IO EmuState
    loop pre = do
      EmuStep{post} <- emulate buttons0 pre
      let EmuState{ticks} = post
      case ticks >= goal of
        False ->  loop post
        True -> do
          let EmuState{mem} = post
          let display = getDisplayFromMem mem
          let !_ = show display
          return post



-- share this... in Mem?
data OnPixel = OnPixel { x :: Int, y :: Int } deriving Show

data Display = Display { onPixels :: [OnPixel] } deriving Show

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
