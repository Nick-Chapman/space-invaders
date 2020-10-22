
module Ports (inputPort,outputPort) where

import Buttons (Buttons,But(..))
import qualified Buttons (get)
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
inputPort1 b =
  makeByte
  ( not (get CoinEntry)
  , get P2start
  , get P1start
  , False
  , get P1shoot
  , get P1left
  , get P1right
  , False
  ) where get but = Buttons.get but b

inputPort2 :: Buttons -> Eff p (Byte p)
inputPort2 b =
  makeByte
  ( get Dip3_livesLow
  , get Dip5_livesHigh
  , get Tilt
  , get Dip6_extraShipEarly
  , get P2shoot
  , get P2left
  , get P2right
  , get Dip7_coinInfoOff
  ) where get but = Buttons.get but b

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
