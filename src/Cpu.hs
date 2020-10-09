
module Cpu (Cpu,Reg(..),RegPair(..),expandRegPair,init,get,set) where

import Prelude hiding (init)
import HiLo (HiLo(..))

data Reg = PCH | PCL | SPH | SPL | A | B | D | E | H | L
  deriving Show

data RegPair = SP | DE | HL
  deriving (Show,Enum,Bounded)

expandRegPair :: RegPair -> HiLo Reg
expandRegPair = \case
  DE -> HiLo {hi = D, lo = E}
  HL -> HiLo {hi = H, lo = L}
  SP -> HiLo {hi = SPH, lo = SPL}

data Cpu b = Cpu
  { pch :: b
  , pcl :: b
  , sph :: b
  , spl :: b
  , regA :: b
  , regB :: b
  , regD :: b
  , regE :: b
  , regH :: b
  , regL :: b
  }

instance Show b => Show (Cpu b) where
  show Cpu{pch,pcl,sph,spl,regA,regB,regD,regE,regH,regL} = unwords
    [ name <> ":" <> v
    | (name,v) <-
      [ ("PC",show pch <> show pcl)
      , ("A", show regA)
      , ("B", show regB)
      , ("D", show regD)
      , ("E", show regE)
      , ("HL", show regH <> show regL)
      , ("SP", show sph <> show spl)
      ]
    ]

init :: b -> Cpu b
init b = Cpu { pch = b, pcl = b, sph = b, spl = b
             , regA = b, regB = b, regD = b, regE = b, regH = b, regL = b }

get :: Cpu b -> Reg -> b
get Cpu{pch,pcl,sph,spl,regA,regB,regD,regE,regH,regL} = \case
  PCH -> pch
  PCL -> pcl
  SPH -> sph
  SPL -> spl
  A -> regA
  B -> regB
  D -> regD
  E -> regE
  H -> regH
  L -> regL

set :: Cpu b -> Reg -> b -> Cpu b
set cpu r x = case r of
  PCH -> cpu { pch = x}
  PCL -> cpu { pcl = x }
  SPH -> cpu { sph = x}
  SPL -> cpu { spl = x }
  A -> cpu { regA = x }
  B -> cpu { regB = x }
  D -> cpu { regD = x }
  E -> cpu { regE = x }
  H -> cpu { regH = x }
  L -> cpu { regL = x }
