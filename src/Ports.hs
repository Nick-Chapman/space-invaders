
module Ports (inputPort,outputPort) where

import Control.Monad (forM_)
import Buttons (But(..))
import Effect (Eff(..))
import Phase (Byte)
import Data.Word8 (Word8)
import Sounds (Sound(..))
import qualified Shifter

inputPort :: Word8 -> Eff p (Byte p)
inputPort = \case
  1 -> inputPort1
  2 -> inputPort2
  3 -> getShifterAtOffset
  n -> UnknownInput n

inputPort1 :: Eff p (Byte p)
inputPort1 = do
  x <- MakeByte 0x0
  x <- GetButton CoinEntry >>= Flip >>= UpdateBit x 0
  x <- GetButton P2start >>= UpdateBit x 1
  x <- GetButton P1start >>= UpdateBit x 2
  x <- GetButton P1shoot >>= UpdateBit x 4
  x <- GetButton P1left  >>= UpdateBit x 5
  x <- GetButton P1right >>= UpdateBit x 6
  return x

inputPort2 :: Eff p (Byte p)
inputPort2 = do
  x <- MakeByte 0x0
  x <- GetButton Dip3_livesLow >>= UpdateBit x 0
  x <- GetButton Dip5_livesHigh >>= UpdateBit x 1
  x <- GetButton Tilt >>= UpdateBit x 2
  x <- GetButton Dip6_extraShipEarly >>= UpdateBit x 3
  x <- GetButton P2shoot >>= UpdateBit x 4
  x <- GetButton P2left >>= UpdateBit x 5
  x <- GetButton P2right >>= UpdateBit x 6
  x <- GetButton Dip7_coinInfoOff >>= UpdateBit x 7
  return x

outputPort :: Word8 -> Byte p -> Eff p ()
outputPort port byte = case port of
  2 -> SetShifterReg Shifter.OFF byte
  3 -> soundFromPort port3 byte
  4 -> fillShifter byte
  5 -> soundFromPort port5 byte
  n -> UnknownOutput n byte

soundFromPort :: (Int -> Sound) -> Byte p -> Eff p ()
soundFromPort soundOfPortBit byte = do
  forM_ [0..4] $ \i -> do -- only 5 of the 8 bits on each port relate to specific sounds
    bit <- TestBit byte i
    let sound = soundOfPortBit i
    SoundControl sound bit

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


fillShifter :: Byte p -> Eff p ()
fillShifter newHi = do
  GetShifterReg Shifter.HI >>= SetShifterReg Shifter.LO
  SetShifterReg Shifter.HI newHi

getShifterAtOffset :: Eff p (Byte p)
getShifterAtOffset = do
  hi <- GetShifterReg Shifter.HI
  lo <- GetShifterReg Shifter.LO
  off <- GetShifterReg Shifter.OFF
  mask <- MakeByte 0x7
  maskedOff1 <- off `AndB` mask
  shiftedHi <- hi `ShiftLeft` maskedOff1
  offBar <- Complement off
  maskedOffBar <- offBar `AndB` mask
  shiftedLo0 <- lo `ShiftRight` maskedOffBar
  one <- MakeByte 0x1
  shiftedLo <- shiftedLo0 `ShiftRight` one
  shiftedHi `OrB` shiftedLo
