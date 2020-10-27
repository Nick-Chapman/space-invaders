
module Sounds(
  Sound(..), allSounds,
  Playing, initPlaying, isSoundPlaying, soundOn, soundOff,
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Sound
  = Ufo
  | Shot
  | PlayerDie
  | InvaderDie
  | ExtraLife
  | FleetMovement1
  | FleetMovement2
  | FleetMovement3
  | FleetMovement4
  | UfoHit
  deriving (Eq,Ord,Show,Enum,Bounded)

allSounds :: [Sound]
allSounds = [toEnum 0..maxBound]

newtype Playing = Playing { map :: Map Sound Bool } deriving Show

initPlaying :: Playing
initPlaying = Playing { map = Map.empty }

isSoundPlaying :: Playing -> Sound -> Bool
isSoundPlaying Playing{map} sound = Map.findWithDefault False sound map

soundOn :: Playing -> Sound -> Playing
soundOn Playing{map} sound = Playing { map = Map.insert sound True map }

soundOff :: Playing -> Sound -> Playing
soundOff Playing{map} sound = Playing { map = Map.insert sound False map }
