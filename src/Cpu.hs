
module Cpu (Cpu,Reg(..),init,get,set) where

import Prelude hiding (init)

data Reg = PCL | PCH
  deriving Show

data Cpu b = Cpu
  { pcl :: b
  , pch :: b
  } deriving Show

init :: b -> Cpu b
init b = Cpu { pcl = b, pch = b }

get :: Cpu b -> Reg -> b
get Cpu{pcl,pch} = \case
  PCL -> pcl
  PCH -> pch

set :: Cpu b -> Reg -> b -> Cpu b
set cpu r x = case r of
  PCL -> cpu { pcl = x }
  PCH -> cpu { pch = x}
