
-- | Everything needed for an emulation...
-- | - Emulation state; Buttons state; Keyboard mapping; Picture generation (game itself + emulator controls)

module World
  ( World, initWorld, stepFrame, soundsToPlay
  , Key(..), KeyMotion(..), updateKey
  , Picture(..), pictureWorld
  ) where

import Addr (Addr(..))
import Buttons (Buttons,buttons0,But(..))
import Data.Bits (testBit)
import Emulate (CB(..),emulate,EmuState(..),Ticks(..))
import Mem (Mem,read)
import Rom (Rom)
import Sounds (Sound(..), allSounds, isSoundPlaying)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import Text.Printf (printf)
import qualified Buttons (get,press,release,toggle)
import qualified Emulate (initState)

data Key
  = KeyInsert
  | F1 | KeyZ | KeyX | KeyEnter
  | F2 | KeyA | KeyS | KeyBackspace
  | F3 | F5 | F6 | F7
  | KeyTab
  | KeySpace
  | KeyDelete
  | KeyEscape
  deriving (Enum,Bounded)

data KeyMotion = Down | Up

data KeyAction
  = NoAction
  | Drive But KeyMotion
  | Toggle But
  | TogglePause
  | ToggleControlDisplay
  | Quit

keyMapping :: (Key,KeyMotion) -> KeyAction
keyMapping = \case
  (KeyInsert,m) -> Drive CoinEntry m
  (F1,m) -> Drive P1start m
  (KeyZ,m) -> Drive P1left m
  (KeyX,m) -> Drive P1right m
  (KeyEnter,m) -> Drive P1shoot m
  (F2,m) -> Drive P2start m
  (KeyA,m) -> Drive P2left m
  (KeyS,m) -> Drive P2right m
  (KeyBackspace,m) -> Drive P2shoot m
  (KeyTab,m) -> Drive Tilt m
  (F3,Down) -> Toggle Dip3_livesLow
  (F5,Down) -> Toggle Dip5_livesHigh
  (F6,Down) -> Toggle Dip6_extraShipEarly
  (F7,Down) -> Toggle Dip7_coinInfoOff
  (KeyDelete,Down) -> TogglePause
  (KeySpace,Down) -> ToggleControlDisplay
  (KeyEscape,Down) -> Quit
  _ -> NoAction

data World = World
  { state :: EmuState
  , buttons :: Buttons
  , paused :: Bool
  , showControls :: Bool
  , frameCount :: Int
  , time :: TimeSpec
  , fps :: Fps
  , soundsToPlay :: [Sound]
  }

initWorld :: Rom -> IO World
initWorld rom = do
  time <- getTime Monotonic
  state <- Emulate.initState rom
  return $ World
    { state
    , buttons = buttons0
    , paused = False
    , showControls = True
    , frameCount = 0
    , time
    , fps = Fps 0
    , soundsToPlay = []
    }

updateKey :: Key -> KeyMotion -> World -> Maybe World
updateKey key motion w@World{showControls,buttons,paused} =
  case keyMapping (key,motion) of
    NoAction -> Just w
    Quit -> Nothing
    TogglePause -> Just $ w { paused = not (paused) }
    ToggleControlDisplay -> Just $ w { showControls = not showControls }
    Drive but motion -> Just $ w { buttons = drive motion but buttons }
    Toggle but -> Just $ w { buttons = Buttons.toggle but buttons }
  where
    drive :: KeyMotion -> But -> Buttons -> Buttons
    drive = \case Down -> Buttons.press; Up -> Buttons.release

stepFrame :: World -> IO World
stepFrame world@World{state=state0,buttons,paused,frameCount} = do
  --putStrLn $ "FRAME: " <> show frameCount
  (if paused then return world else loop state0)
    >>= measureFps
  where

    cb :: CB
    cb = CB { traceI = \_ _ -> return () }

    loop :: EmuState -> IO World
    loop pre = do
      post <- emulate cb buttons pre
      case reachFrame pre post of
        False -> loop post
        True -> do
          let soundsToPlay = soundsWhichSwitchOn state0 post
          return $ world { frameCount = frameCount + 1 , state = post, soundsToPlay }

soundsWhichSwitchOn :: EmuState -> EmuState -> [Sound]
soundsWhichSwitchOn EmuState{playing=p0} EmuState{playing=p1} =
  [ s
  | s <- allSounds
  , not (isSoundPlaying p0 s)
  , isSoundPlaying p1 s
  ]

reachFrame :: EmuState -> EmuState -> Bool -- TODO: there a better way than this!
reachFrame s0 s1 = do
  let EmuState{ticks=ticks0} = s0
  let EmuState{ticks=ticks1} = s1
  let f0 = unTicks ticks0 `div` cyclesPerFrame
  let f1 = unTicks ticks1 `div` cyclesPerFrame
  let yes = f1 > f0
  yes
  where cyclesPerFrame = 33333

data Picture
  = Pictures [Picture]
  | Text { string :: String, lineNo :: Int, emphasized :: Bool }
  | Pixel { x :: Int, y :: Int }

pictureWorld :: World -> Picture
pictureWorld w@World{state=EmuState{mem},showControls,frameCount,fps} =
  Pictures
  [ pictureVideoMem mem
  , if showControls then controls else Pictures []
  ]
  where
    controls = Pictures
      [ pictureButtons w
      , Text { lineNo = 1, string = "frame : " <> show frameCount, emphasized = False }
      , Text { lineNo = 2, string = "fps : " <> show fps, emphasized = False }
      ]

pictureVideoMem :: Mem -> Picture
pictureVideoMem mem = do
  Pictures
    [ Pixel {x = x, y = 256 - y} -- FLIP HERE
    | x :: Int <- [0..223]
    , yByte <- [0..31]
    , let byte = Mem.read mem (Addr (fromIntegral (0x2400 + x * 32 + yByte)))
    , yBit <- [0..7]
    , byte `testBit` yBit
    , let y  = 8 * yByte + yBit
    ]

pictureButtons :: World -> Picture
pictureButtons World{buttons,paused} =
  Pictures [ Text { lineNo, string = describeKeyAndMapping key, emphasized }
           | (lineNo,key) <- zip [4..] keys
           , let emphasized = do
                   let action = keyMapping (key,Down)
                   case action of
                     TogglePause -> paused
                     Toggle but -> Buttons.get but buttons
                     Drive but _ -> Buttons.get but buttons
                     _ -> False
           ]
  where keys = [minBound..maxBound]

describeKeyAndMapping :: Key -> String
describeKeyAndMapping key = show key <> " : " <> show (keyMapping (key,Down))

instance Show Key where
  show = \case
    KeyEscape -> "[escape]"
    KeyDelete -> "[delete]"
    KeySpace -> "[space]"
    KeyInsert -> "[insert]"
    KeyTab -> "[tab]"
    KeyEnter -> "[enter]"
    KeyBackspace -> "[backspace]"
    KeyZ -> "Z"
    KeyX -> "X"
    KeyA -> "A"
    KeyS -> "S"
    F1 -> "F1"
    F2 -> "F2"
    F3 -> "F3"
    F5 -> "F5"
    F6 -> "F6"
    F7 -> "F7"

instance Show KeyAction where
  show = \case
    NoAction -> "NO-ACTION"
    Quit -> "QUIT"
    TogglePause -> "PAUSE"
    ToggleControlDisplay -> "SHOW-KEYS"
    Drive but _ -> show but
    Toggle but -> show but


newtype Fps = Fps Double

instance Show Fps where show (Fps f) = printf "%.00g" f

measureFps :: World -> IO World
measureFps world@World{time=last, fps=fpsLast} = do
  now <- getTime Monotonic
  let fpsNow = makeFps last now
  let fpsNowSmoothed = smoothFps fpsLast fpsNow
  return $ world { time = now, fps = fpsNowSmoothed }

makeFps :: TimeSpec -> TimeSpec -> Fps
makeFps last now = do
  let TimeSpec{sec,nsec} = now - last
  let duration = gig * sec + nsec
  Fps (fromIntegral gig / fromIntegral duration)
    where gig = 1_000_000_000

smoothFps :: Fps -> Fps -> Fps
smoothFps (Fps last) (Fps now) = Fps $ last * decay + now * (1 - decay)
    where
      decay = 0.9
