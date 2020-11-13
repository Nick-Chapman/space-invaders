
module Tst (main) where

import Buttons (buttons0)
import Byte (Byte(..))
import Cpu (Cpu(..),Reg(..),set)
import Data.Bits (bit)
import Data.List (intercalate)
import Emulate (Bit(..),EmuState(..),EmuStep(..),initState,emulate,Ticks(..),prettyPrefix)
import InstructionSet (Instruction,prettyInstructionBytes)
import Mem (Mem,initTst)
import Prelude hiding (init)
import System.IO (Handle,hPutStrLn)
import qualified Data.ByteString as BS (readFile,unpack)

main :: Handle -> IO ()
main handle = do
  mem <- loadTestMem "roms/TST8080.COM"
  let state0 = initState mem
  let EmuState{cpu=cpu0} = state0
  let cpu1 = Cpu.set cpu0 PCH 0x1
  let state1 = state0 { cpu = cpu1 }
  trace handle state1

loadTestMem :: FilePath -> IO Mem
loadTestMem path = do
  let offset = 0x100
  byteString <- BS.readFile path
  let bytes = map Byte (BS.unpack byteString)
  let front =
        (patch 0x7 0xc9
         . patch 0x5 0xd3
         . patch 0x6 0x01
         . patch 0x0 0xd3) (take offset (repeat (Byte 0)))
  return $ Mem.initTst (front ++ bytes)
    where
      patch :: Int -> Byte -> [Byte] -> [Byte]
      patch i b xs =
        [ y
        | (j,x) <- zip [0..] xs
        , let y = if i==j then b else x
        ]

trace :: Handle -> EmuState -> IO ()
trace handle = do
  loop 0
  where
    loop :: Int -> EmuState -> IO ()
    loop i pre = do
      if (i>650) then return () else do
        EmuStep{instruction,post} <- emulate buttons0 pre
        hPutStrLn handle (seeState pre ++ prettyStep pre instruction)
        loop (i+1) post


seeState :: EmuState -> String
seeState EmuState{cpu=Cpu{pch,pcl,sph,spl,regA,regB,regC,regD,regE,regH,regL
                         ,flagS,flagZ,flagA,flagP,flagCY
                         },ticks} = do
  intercalate ", "
    [ name <> ": " <> v
    | (name,v) <-
      [ ("PC",show pch <> show pcl)
      , ("AF", show regA <> show regF)
      , ("BC", show regB <> show regC)
      , ("DE", show regD <> show regE)
      , ("HL", show regH <> show regL)
      , ("SP", show sph <> show spl)
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
