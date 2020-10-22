
module Buttons
  ( Buttons, buttons0
  , But(..), get, set, press, release, toggle,
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

data But
  = CoinEntry
  | Tilt
  | P1start
  | P1left
  | P1right
  | P1shoot
  | P2start
  | P2left
  | P2right
  | P2shoot
  | Dip3_livesLow
  | Dip5_livesHigh
  | Dip6_extraShipEarly
  | Dip7_coinInfoOff
  deriving (Eq,Ord)

instance Show But where
  show = \case
    CoinEntry -> "coin entry"
    Tilt -> "TILT"
    P1start -> "player1 start"
    P1left -> "player1 left"
    P1right -> "player1 right"
    P1shoot -> "player1 shoot"
    P2start -> "player2 start"
    P2left -> "player2 left"
    P2right -> "player2 right"
    P2shoot -> "player2 shoot"
    Dip3_livesLow -> "[dip3] lives (3,4,5,6) lsb"
    Dip5_livesHigh -> "[dip5] lives (3,4,5,6) msb"
    Dip6_extraShipEarly -> "[dip6] extra ship at 1000"
    Dip7_coinInfoOff -> "[dip7] coin info off"


newtype Buttons = Buttons { map :: Map But Bool } deriving Show

buttons0 :: Buttons
buttons0 = Buttons { map = Map.empty }

get :: But -> Buttons -> Bool
get but Buttons{map} = Map.findWithDefault False but map

set :: Bool -> But -> Buttons -> Buttons
set v but Buttons{map} = Buttons { map = Map.insert but v map }

press :: But -> Buttons -> Buttons
press = set True

release :: But -> Buttons -> Buttons
release = set False

toggle :: But -> Buttons -> Buttons
toggle b buttons = set (not (get b buttons)) b buttons
