
module Cpu (
  Cpu,Reg(..),RegPair(..),
  expandRegPair,
  init,
  get,set,
  getFlagZ,setFlagZ,
  ) where

import Prelude hiding (init)
import HiLo (HiLo(..))

data Reg = PCH | PCL | SPH | SPL | A | B | C | D | E | H | L | FLAGS
  deriving (Eq,Ord,Show)

data RegPair = BC | DE | HL | SP | PSW
  deriving (Eq,Ord,Show)

expandRegPair :: RegPair -> HiLo Reg
expandRegPair = \case
  BC -> HiLo {hi = B, lo = C}
  DE -> HiLo {hi = D, lo = E}
  HL -> HiLo {hi = H, lo = L}
  SP -> HiLo {hi = SPH, lo = SPL}
  PSW -> HiLo {hi = A, lo = FLAGS}

data Cpu b = Cpu
  { pch :: b
  , pcl :: b
  , sph :: b
  , spl :: b
  , regA :: b
  , regB :: b
  , regC :: b
  , regD :: b
  , regE :: b
  , regH :: b
  , regL :: b
  , flagZ :: b -- The byte which should be tested against zero
  , regFlags :: b
  }

instance Show b => Show (Cpu b) where
  show Cpu{pch,pcl,sph,spl,regA,regB,regC,regD,regE,regH,regL,regFlags} = unwords
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
      , ("FLAGS", show regFlags)
      ]
    ]

init :: b -> Cpu b
init b = Cpu { pch = b, pcl = b, sph = b, spl = b
             , regA = b, regB = b, regC = b, regD = b, regE = b, regH = b, regL = b
             , flagZ = b
             , regFlags = b
             }

getFlagZ :: Cpu b -> b
getFlagZ Cpu{flagZ} = flagZ

setFlagZ :: Cpu b -> b -> Cpu b
setFlagZ cpu b = cpu { flagZ = b }


get :: Cpu b -> Reg -> b
get Cpu{pch,pcl,sph,spl,regA,regB,regC,regD,regE,regH,regL,regFlags} = \case
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
  FLAGS -> regFlags

set :: Cpu b -> Reg -> b -> Cpu b
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
  FLAGS -> cpu { regFlags = x }
