
module TraceEmu (traceEmulate) where

import Control.Monad (when)
import Data.Bits (testBit)
import Text.Printf (printf)

import Addr (Addr(..))
import Byte (Byte)
import Cpu (Reg(PCL,PCH))
import Emulate (emulate,Emulation(..),EmuState(..),Ticks(..))
import HiLo (HiLo(..))
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem,read)
import qualified Addr (fromHiLo)
import qualified Cpu

traceEmulate :: Bool -> Mem -> IO ()
traceEmulate traceOn mem = emulate mem >>= loop
  where
    loop :: Emulation -> IO ()
    loop = \case
      CrashDecode s byte -> do
        let pc = programCounter s - 1
        let EmuState{icount} = s
        error $ "CrashDecode, icount = " <> show icount <> ", pc = " <> show pc <> " : " <> show byte
      EmuStep
        { pre
        , instruction
        , post = post@EmuState{cpu,icount}
        , continue
        } -> do
        let poi = Just 1763139
        let debug = case poi of Just poi -> (icount >= poi - 5); Nothing -> False
        when (traceOn || debug) $
          putStrLn (ljust 60 (prettyStep pre instruction) ++ show cpu)
        printWhenNewFrame pre post
        when (traceOn && icount > 50000) $ error "STOP"
        case poi of Just poi -> when (icount > poi + 5) $ error "STOP2"; Nothing -> return ()
        continue >>= loop

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

rjust :: Int -> String -> String
rjust n s = take (max 0 (n - length s)) (repeat ' ') <> s

programCounter :: EmuState -> Addr
programCounter EmuState{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}

prettyStep :: EmuState -> Instruction Byte -> String
prettyStep s i = do
  let pc = programCounter s
  unwords
    [ prettyTicks s
    , show pc
    , ":"
    , ljust 10 (prettyInstructionBytes i)
    , show i
    ]

prettyTicks :: EmuState -> String
prettyTicks EmuState{ticks,icount} =
  unwords [ printf "%8d" icount, rjust 11 (show ticks) ]

printWhenNewFrame :: EmuState -> EmuState -> IO ()
printWhenNewFrame s0 s1 = do
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
    putStrLn $ unwords
      [ prettyTicks s1
      , printf "FRAME{%d}" f1
      , printf "#onPixels = %d" (length pixs)
      --, printf "(cycles-late=%d)" _nCyclesLate
      --, printf "(frame-skipped=%d)" _nFramesSkipped
      --, show (f0,f1)
      --, show (ticks0,ticks1)
      ]
    --where cyclesPerFrame = 1000
    where cyclesPerFrame = 33333

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
