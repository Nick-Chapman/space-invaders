
module Cpu (
  Cpu,Reg(..),RegPair(..),Flag(..),
  init,
  get,set,
  getFlag,setFlag,
  ) where

import Prelude hiding (init)
import Phase (Byte,Bit)

data Reg = PCH | PCL | SPH | SPL | A | B | C | D | E | H | L
  deriving (Eq,Ord,Show)

data RegPair = BC | DE | HL | SP | PSW
  deriving (Eq,Ord,Show)

data Flag = Z | CY

data Cpu p = Cpu
  { pch :: Byte p
  , pcl :: Byte p
  , sph :: Byte p
  , spl :: Byte p
  , regA :: Byte p
  , regB :: Byte p
  , regC :: Byte p
  , regD :: Byte p
  , regE :: Byte p
  , regH :: Byte p
  , regL :: Byte p
  , flagZ :: Bit p
  , flagCY :: Bit p
  }

instance (Show (Bit p), Show (Byte p)) => Show (Cpu p) where
  show Cpu{pch,pcl,sph,spl,regA,regB,regC,regD,regE,regH,regL
          ,flagZ,flagCY} = unwords
    [ name <> ":" <> v
    | (name,v) <-
      [ ("PC",show pch <> show pcl)
      , ("A", show regA)
      , ("B", show regB)
      , ("C", show regC)
      , ("D", show regD)
      , ("E", show regE)
      , ("HL", show regH <> show regL)
      , ("SP", show sph <> show spl)
      , ("Z", show flagZ)
      , ("CY", show flagCY)
      ]
    ]


init :: Byte p -> Bit p -> Cpu p
init b bit0 =
  Cpu { pch = b, pcl = b, sph = b, spl = b
      , regA = b, regB = b, regC = b, regD = b, regE = b, regH = b, regL = b
      , flagZ = bit0, flagCY = bit0
      }


getFlag :: Cpu p -> Flag -> Bit p
getFlag Cpu{flagZ,flagCY} = \case
  Z -> flagZ
  CY -> flagCY

setFlag :: Cpu p -> Flag -> Bit p -> Cpu p
setFlag cpu flag x = case flag of
  Z -> cpu { flagZ = x }
  CY -> cpu { flagCY = x }


get :: Cpu p -> Reg -> Byte p
get Cpu{pch,pcl,sph,spl,regA,regB,regC,regD,regE,regH,regL} = \case
  PCH -> pch
  PCL -> pcl
  SPH -> sph
  SPL -> spl
  A -> regA
  B -> regB
  C -> regC
  D -> regD
  E -> regE
  H -> regH
  L -> regL

set :: Cpu p -> Reg -> Byte p -> Cpu p
set cpu r x = case r of
  PCH -> cpu { pch = x}
  PCL -> cpu { pcl = x }
  SPH -> cpu { sph = x}
  SPL -> cpu { spl = x }
  A -> cpu { regA = x }
  B -> cpu { regB = x }
  C -> cpu { regC = x }
  D -> cpu { regD = x }
  E -> cpu { regE = x }
  H -> cpu { regH = x }
  L -> cpu { regL = x }
