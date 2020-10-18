
module Ports (inputPort,outputPort) where

import Buttons (Buttons(..),Lives(..),Bonus(..))
import Effect (Eff(..))
import Phase (Byte)
import Data.Word8 (Word8)

inputPort :: Word8 -> Eff p (Byte p)
inputPort = \case
  1 -> GetButtons >>= inputPort1
  2 -> GetButtons >>= inputPort2
  3 -> GetShiftRegisterAtOffset
  n -> Unimplemented ("IN:" <> show n)

inputPort1 :: Buttons -> Eff p (Byte p)
inputPort1 Buttons{coin,p2start,p1start,p1shoot,p1left,p1right} =
  makeByte
  ( not coin
  , p2start
  , p1start
  , False
  , p1shoot
  , p1left
  , p1right
  , False
  )

inputPort2 :: Buttons -> Eff p (Byte p)
inputPort2 Buttons{lives,tilt,bonus,p2shoot,p2left,p2right,coinInfoOff} =
  makeByte
  ( livesLow
  , livesHigh
  , tilt
  , case bonus of BonusAt1000 -> True; BonusAt1500 -> False
  , p2shoot
  , p2left
  , p2right
  , coinInfoOff
  )
  where
    (livesHigh,livesLow) = case lives of
      Lives3 -> (False,False)
      Lives4 -> (False,True)
      Lives5 -> (True,False)
      Lives6 -> (True,True)

makeByte :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Eff p (Byte p)
makeByte (a,b,c,d,e,f,g,h) = MakeByte $ sum
  [ mk a 1
  , mk b 2
  , mk c 4
  , mk d 8
  , mk e 16
  , mk f 32
  , mk g 64
  , mk h 128
  ]
  where mk x v = if x then v else 0


outputPort :: Word8 -> Byte p -> Eff p ()
outputPort port byte = case port of
  2 -> SetShiftRegisterOffset byte
  3 -> Sound
  4 -> FillShiftRegister byte
  5 -> Sound
  6 -> return () -- ignore watchdog
  n -> Unimplemented ("OUT:" <> show n)