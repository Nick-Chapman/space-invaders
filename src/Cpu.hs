
module Cpu (Cpu,Reg(..),init,get,set) where

import Prelude hiding (init)

data Reg = PCH | PCL | SPH | SPL | RegB | RegD | RegE | RegH | RegL
  deriving Show

data Cpu b = Cpu
  { pch :: b
  , pcl :: b
  , sph :: b
  , spl :: b
  , regB :: b
  , regD :: b
  , regE :: b
  , regH :: b
  , regL :: b
  }

instance Show b => Show (Cpu b) where
  show Cpu{pch,pcl,sph,spl,regB,regD,regE,regH,regL} = unwords
    [ name <> ":" <> v
    | (name,v) <-
      [ ("PC",show pch <> show pcl)
      , ("B", show regB)
      , ("D", show regD)
      , ("E", show regE)
      , ("HL", show regH <> show regL)
      , ("SP", show sph <> show spl)
      ]
    ]

init :: b -> Cpu b
init b = Cpu { pch = b, pcl = b, sph = b, spl = b, regB = b, regD = b, regE = b, regH = b, regL = b }

get :: Cpu b -> Reg -> b
get Cpu{pch,pcl,sph,spl,regB,regD,regE,regH,regL} = \case
  PCH -> pch
  PCL -> pcl
  SPH -> sph
  SPL -> spl
  RegB -> regB
  RegD -> regD
  RegE -> regE
  RegH -> regH
  RegL -> regL

set :: Cpu b -> Reg -> b -> Cpu b
set cpu r x = case r of
  PCH -> cpu { pch = x}
  PCL -> cpu { pcl = x }
  SPH -> cpu { sph = x}
  SPL -> cpu { spl = x }
  RegB -> cpu { regB = x }
  RegD -> cpu { regD = x }
  RegE -> cpu { regE = x }
  RegH -> cpu { regH = x }
  RegL -> cpu { regL = x }
