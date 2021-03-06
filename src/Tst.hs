
module Tst (main) where

import Buttons (buttons0)
import Byte (Byte(..))
import Cpu (Cpu(..),Reg(..),set)
import Data.Bits (bit)
import Data.List (intercalate)
import Emulate (Bit(..),EmuState(..),initState,CB(..),emulate,Ticks(..),prettyPrefix)
import InstructionSet (Instruction,prettyInstructionBytes)
import Prelude hiding (init)
import Rom (Rom)
import System.IO (Handle,hPutStrLn)
import qualified Data.ByteString as BS (readFile,unpack)
import qualified Rom (fromBytes)

main :: Handle -> IO ()
main handle = do
  rom <- loadTestRom "roms/TST8080.COM"
  state0 <- initState rom
  let EmuState{cpu=cpu0} = state0
  let cpu1 = Cpu.set cpu0 PCH 0x1
  let state1 = state0 { cpu = cpu1 }
  trace handle state1

loadTestRom :: FilePath -> IO Rom
loadTestRom path = do
  let offset = 0x100
  byteString <- BS.readFile path
  let bytes0 = map Byte (BS.unpack byteString)
  let bytes = (reverse . dropWhile (== Byte 0) . reverse) bytes0
  let front =
        (patch 0x7 0xc9
         . patch 0x5 0xd3
         . patch 0x6 0x01
         . patch 0x0 0xd3) (take offset (repeat (Byte 0)))
  return $ Rom.fromBytes (front ++ bytes)
    where
      patch :: Int -> Byte -> [Byte] -> [Byte]
      patch i b xs =
        [ y
        | (j,x) <- zip [0..] xs
        , let y = if i==j then b else x
        ]

trace :: Handle -> EmuState -> IO ()
trace handle = loop
  where
    max = 650
    traceI :: EmuState -> Instruction Byte -> IO ()
    traceI s@EmuState{icount=i} instruction = do
      if (i>max) then return () else do
        hPutStrLn handle (seeState s ++ prettyStep s instruction)

    cb = CB { traceI = Just traceI }

    loop :: EmuState -> IO ()
    loop s@EmuState{icount=i} = do
      if (i>max) then return () else do
        emulate cb buttons0 s >>= loop


seeState :: EmuState -> String
seeState EmuState{cpu=Cpu{pch,pcl,hl,sp,regA,regB,regC,regD,regE
                         ,flagS,flagZ,flagA,flagP,flagCY
                         },ticks} = do
  intercalate ", "
    [ name <> ": " <> v
    | (name,v) <-
      [ ("PC",show pch <> show pcl)
      , ("AF", show regA <> show regF)
      , ("BC", show regB <> show regC)
      , ("DE", show regD <> show regE)
      , ("HL", show hl)
      , ("SP", show sp)
      , ("CYC", show (unTicks ticks))
      ]
    ]
    where
      regF = Byte $ sum [ if b then bit i else 0 | (i,Bit b) <- zip [0..] (reverse flags) ]
      flags = [flagS, flagZ, Bit False, flagA, Bit False, flagP, Bit True, flagCY]

prettyStep :: EmuState -> Instruction Byte -> String
prettyStep s i =
  prettyPrefix s $
  unwords [ ljust 10 (prettyInstructionBytes i), show i ]

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')
