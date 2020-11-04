
module Ports (inputPort,outputPort) where

import Control.Monad (forM_)
import Buttons (Buttons,But(..))
import qualified Buttons (get)
import Effect (Eff(..))
import Phase (Byte)
import Data.Word8 (Word8)
import Sounds (Sound(..))

inputPort :: Word8 -> Eff p (Byte p)
inputPort = \case
  1 -> GetButtons >>= inputPort1
  2 -> GetButtons >>= inputPort2
  3 -> GetShiftRegisterAtOffset
--  n -> Unimplemented ("IN:" <> show n)
  n -> UnknownInput n

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
  3 -> soundFromPort port3 byte
  4 -> FillShiftRegister byte
  5 -> soundFromPort port5 byte
  6 -> return () -- ignore watchdog
  0 -> return () -- ignore for tst
  1 -> return () -- ignore for tst
--  n -> Unimplemented ("OUT:" <> show n)
  n -> UnknownOutput n

soundFromPort :: (Int -> Sound) -> Byte p -> Eff p ()
soundFromPort soundOfPortBit byte = do
  forM_ [0..4] $ \i -> do -- only 5 of the 8 bits on each port relate to specific sounds
    bit <- TestBit byte i
    let sound = soundOfPortBit i
    CaseBit bit >>= \case
      True -> SoundOn sound
      False -> SoundOff sound

port3 :: Int -> Sound
port3 = \case
  0 -> Ufo
  1 -> Shot
  2 -> PlayerDie
  3 -> InvaderDie
  4 -> ExtraLife
  n -> error $ "unknown port3 sound bit: " <> show n

port5 :: Int -> Sound
port5 = \case
  0 -> FleetMovement1
  1 -> FleetMovement2
  2 -> FleetMovement3
  3 -> FleetMovement4
  4 -> UfoHit
  n -> error $ "unknown port5 sound bit: " <> show n
