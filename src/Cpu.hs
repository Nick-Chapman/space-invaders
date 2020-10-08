
module Cpu (Cpu,Reg(..),init,get,set) where

import Prelude hiding (init)

data Reg = PCH | PCL | SPH | SPL
  deriving Show

data Cpu b = Cpu
  { pch :: b
  , pcl :: b
  , sph :: b
  , spl :: b
  } deriving Show

init :: b -> Cpu b
init b = Cpu { pch = b, pcl = b, sph = b, spl = b }

get :: Cpu b -> Reg -> b
get Cpu{pch,pcl,sph,spl} = \case
  PCH -> pch
  PCL -> pcl
  SPH -> sph
  SPL -> spl

set :: Cpu b -> Reg -> b -> Cpu b
set cpu r x = case r of
  PCH -> cpu { pch = x}
  PCL -> cpu { pcl = x }
  SPH -> cpu { sph = x}
  SPL -> cpu { spl = x }
