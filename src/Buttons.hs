
module Buttons where

data Lives = Lives3 | Lives4 | Lives5 | Lives6
data Bonus = BonusAt1000 | BonusAt1500

data Buttons = Buttons
  { coin :: Bool
  , p2start :: Bool
  , p1start :: Bool
  , p1shoot :: Bool
  , p1left :: Bool
  , p1right :: Bool
  , lives :: Lives
  , tilt :: Bool
  , bonus :: Bonus
  , p2shoot :: Bool
  , p2left :: Bool
  , p2right :: Bool
  , coinInfoOff :: Bool
  }

buttons0 :: Buttons
buttons0 = Buttons
  { coin = True -- False -- TODO: change to False, and update expected state
  , p2start = False
  , p1start = False
  , p1shoot = False
  , p1left = False
  , p1right = False
  , lives = Lives3
  , tilt = False
  , bonus = BonusAt1500
  , p2shoot = False
  , p2left = False
  , p2right = False
  , coinInfoOff = False
  }
