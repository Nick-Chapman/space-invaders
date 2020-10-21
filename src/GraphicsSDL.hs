
module GraphicsSDL (main) where

import Data.Bits (testBit)

import Control.Monad (when,unless)
import Foreign.C.Types (CInt)
import System.IO (hFlush,stdout)
import qualified Data.Text as Text (pack)

import Addr (Addr(..))
import Buttons (Buttons(..),buttons0)
import Mem (Mem,read)
import Emulate (EmuStep(..),emulate,EmuState(..),state0,Ticks(..))

--import GHC.Word (Word8)

import SDL (($=))

import SDL.Font as Font

import qualified SDL (
  Window,
  Renderer,
  --WindowConfig,
  Event(Event),
  EventPayload(QuitEvent),
  --RendererConfig(RendererConfig,rendererType,rendererTargetTexture),
  --RendererType(AcceleratedVSyncRenderer),
  Rectangle(Rectangle),
  Point(P),
  V2(V2),
  V4(V4),
  initializeAll,
  createWindow,
  --showWindow,
  defaultRenderer,
  createRenderer,
  present,
--  pollEvent,
  pollEvents,
  destroyRenderer,
  destroyWindow,
  quit,
  clear,
  fillRect,
  rendererDrawColor,
  defaultWindow,
  windowInitialSize,
  )

import qualified SDL


import SDL.Font
import SDL
import Data.Text



screenWidth,screenHeight::CInt
screenWidth = 224
screenHeight = 256

sc :: CInt
sc = 3

scale :: CInt -> CInt
scale x = fromIntegral (sc * x)

main :: Mem -> IO ()
main mem = do
  SDL.initializeAll
  SDL.Font.initialize

  let w = scale screenWidth + 300
  let h = scale screenHeight + 50

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 w h }

  win :: SDL.Window <- SDL.createWindow (Text.pack "Space Invaders") $ winConfig
  --SDL.showWindow win -- need?

  r <- SDL.createRenderer win (-1) SDL.defaultRenderer

  font :: Font <- Font.load "assets/Acorn Full Nostalgia.ttf" 20
  
  let

    draw :: World -> IO ()
    draw world = do

      setColor r DarkGrey
      SDL.clear r
      setColor r White
      renderPixels r world

      renderKeyPicture r font world

      SDL.present r


    loop :: Int -> World -> IO ()
    loop n world = do

      when (n `mod` 60 == 0) $
        putStr $ show (n `div` 60)

      putStr "."; flush

      let loopEvents world =
            \case
              [] -> Just world
              e:es -> do
                case processEvent e world of
                  Nothing -> Nothing -- quit
                  Just world -> loopEvents world es


      es :: [SDL.Event] <- SDL.pollEvents

      let worldMaybe = loopEvents world es

      case worldMaybe of
        Nothing -> return () -- quitting
        Just world -> do

          draw world
          world <- doStep world
          --SDL.delay 10
          loop (n+1) world

  world0 <- initWorld mem
  loop 0 world0

  print "destroyRenderer"
  SDL.destroyRenderer r
  print "destroyWindow"
  SDL.destroyWindow win
  print "quit"
  SDL.quit


flush :: IO ()
flush = hFlush stdout


data Colour = Black | White | Red | Blue | Green | Yellow | DarkGrey

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r DarkGrey  = SDL.rendererDrawColor r $= SDL.V4 50 50 50 maxBound
setColor r Black  = SDL.rendererDrawColor r $= SDL.V4 0 0 0 maxBound
setColor r White  = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red    = SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r $= SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound




processEvent :: SDL.Event -> World -> Maybe World -- Nonw mens quite
processEvent e s = case e of
  SDL.Event _t SDL.QuitEvent -> Nothing
  SDL.Event _ (SDL.KeyboardEvent ke) -> Just (processKeyboardEvent ke s)
  SDL.Event _ _ -> Just s

processKeyboardEvent :: SDL.KeyboardEventData -> World -> World
processKeyboardEvent ke s =  do
  let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
  let motion = SDL.keyboardEventKeyMotion ke
  case (keycode,motion) of
    (SDL.KeycodeZ, SDL.Pressed) -> s { keyState = (keyState s) { keyZ = True } }
    (SDL.KeycodeZ, SDL.Released) -> s { keyState = (keyState s) { keyZ = False } }
    (SDL.KeycodeX, SDL.Pressed) -> s { keyState = (keyState s) { keyX = True } }
    (SDL.KeycodeX, SDL.Released) -> s { keyState = (keyState s) { keyX = False } }

    (SDL.KeycodeReturn, SDL.Pressed) -> s { keyState = (keyState s) { keyRet = True } }
    (SDL.KeycodeReturn, SDL.Released) -> s { keyState = (keyState s) { keyRet = False } }

    (SDL.KeycodeInsert, SDL.Pressed) -> s { keyState = (keyState s) { keyInsert = True } }
    (SDL.KeycodeInsert, SDL.Released) -> s { keyState = (keyState s) { keyInsert = False } }
    (SDL.KeycodeTab, SDL.Pressed) -> s { keyState = (keyState s) { keyTab = True } }
    (SDL.KeycodeTab, SDL.Released) -> s { keyState = (keyState s) { keyTab = False } }

    (SDL.KeycodeF1, SDL.Pressed) ->
      s { keyState = (keyState s) { dip1 = not (dip1 (keyState s))  } }

    _ -> s


----------------------------------------------------------------------

data KeyState = KeyState
  { keyZ :: Bool
  , keyX :: Bool
  , keyRet :: Bool
  , keyTab :: Bool
  , keyInsert :: Bool
  , dip1 :: Bool
  }

ks0 :: KeyState
ks0 = KeyState
  { keyZ = False
  , keyX = False
  , keyRet = False
  , keyTab = False
  , keyInsert = False
  , dip1 = False
  }


renderKeyPicture :: SDL.Renderer -> Font -> World -> IO ()
renderKeyPicture r font = \case
  World{keyState=KeyState{keyZ,keyX,keyRet,keyInsert,keyTab,dip1}} -> do
    renderKeyPic r ("left", "z",250,10,keyZ)
    renderKeyPic r ("right", "x",250,50,keyX)
    renderKeyPic r ("fire", "[enter]",250,90,keyRet)
    renderKeyPic r ("[dip1]", "F1",250,130,dip1)
    renderKeyPic r ("coin", "[insert]",250,170,keyInsert)
    renderKeyPic r ("start", "[tab]",250,210,keyTab)
  where
    renderKeyPic :: SDL.Renderer -> (String,String,CInt,CInt,Bool)-> IO ()
    renderKeyPic r (action,keyname,x,y,pressed) = do
      setColor r (if pressed then Blue else Yellow)
      SDL.fillRect r (Just rect)
      renderSolidText r font red keyname (V2 (fromIntegral (scale x)) (fromIntegral (scale y))) False
      renderSolidText r font red action (V2 (fromIntegral (scale x)) (fromIntegral (scale y + 20))) False
      where
        rect = SDL.Rectangle (SDL.P (SDL.V2 (scale x) (scale y))) (SDL.V2 (2*size) size)
        size = scale 30

        red = SDL.V4 maxBound 0 0 maxBound



renderSolidText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> V2 Double -> Bool -> IO ()
renderSolidText r fo c s p = renderText r fo (SDL.Font.solid fo) c s (toCIntV2 p)


renderText :: SDL.Renderer -> SDL.Font.Font -> (SDL.Font.Color -> Data.Text.Text -> IO SDL.Surface) ->
           SDL.Font.Color -> String -> V2 CInt -> Bool -> IO ()
renderText r fo fu c t (V2 x y) center = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  (fontSizeW,fontSizeH) <- SDL.Font.size fo text
  let (w, h) = (fromIntegral fontSizeW, fromIntegral fontSizeH)
  unless center $
    SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x y) (V2 w h)))
  when center $ do
    let x' = x - fromIntegral (fontSizeW `div` 2)
    SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x' y) (V2 w h)))
  SDL.destroyTexture texture

toCIntV2 :: V2 Double -> V2 CInt
toCIntV2 (V2 x y) = V2 (floor x) (floor y)


----------------------------------------------------------------------

data World = World
  { --pixels :: Pixels
   keyState :: KeyState
--  , buttons :: Buttons
  , frameCount :: Int
  , state :: EmuState
  }

initWorld :: Mem -> IO World
initWorld mem = return $ World
  { --pixels = pixels0
   keyState = ks0
  , frameCount = 0
  , state = state0 mem
  }

{-
pixels0 :: Pixels
pixels0 = do
  let m = 10  -- + n `mod` 5
  let ps = [ (x,y)
           | x <- [0..224-1]
           , y <- [0..256-1]
           , (x `mod` m == 0 ) || (y `mod` m == 0)
           ]
  Pixels ps
-}

----------------------------------------------------------------------

--data Pixels = Pixels [(CInt,CInt)]

renderPixels :: SDL.Renderer -> World -> IO ()
renderPixels r world =
  sequence_ [SDL.fillRect r (Just (mkRect (scale (fromIntegral x)) (scale (fromIntegral y)) (scale 1) (scale 1)))
            | OnPixel{x,y} <- ps ]
  where ps = pixelsOfWorld world


mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
  where o = SDL.P (SDL.V2 x y)
        s = SDL.V2 w h


----------------------------------------------------------------------

doStep :: World -> IO World
doStep s = updateWorld s

updateWorld :: World -> IO World
updateWorld world@World{keyState,frameCount,state=state0} = do
  putStr "."; flush
  w <- loop state0
  --let size = length (show w)
  --putStr (show size <> "]"); flush
  return w
  where
    loop :: EmuState -> IO World
    loop pre = do
      let buttons = buttonsOfKeystate keyState
      EmuStep{post} <- emulate buttons pre
      case reachFrame pre post of
        False -> loop post
        True -> do
          --let EmuState{mem} = post
          return $ world
            { frameCount = frameCount + 1
            --, buttons
            -- , disp = getDisplayFromMem mem
            , state = post
            }


buttonsOfKeystate :: KeyState -> Buttons
buttonsOfKeystate KeyState{keyZ,keyX,keyRet,keyTab,keyInsert} =
  buttons0
  { p1left = keyZ
  , p1right = keyX
  , p1shoot = keyRet
  , p1start = keyTab
  , coin = keyInsert
  }


pixelsOfWorld :: World -> [OnPixel]
pixelsOfWorld World{state} = do
  let EmuState{mem} = state
  let Disp{onPixels} = getDisplayFromMem mem
  onPixels

data Disp = Disp { onPixels :: [OnPixel] } deriving Show

data OnPixel = OnPixel { x :: Int, y :: Int } deriving Show

getDisplayFromMem :: Mem -> Disp
getDisplayFromMem mem = do
  Disp
    [ OnPixel {x = x, y = 256 - y} -- FLIP HERE
    | x :: Int <- [0..223]
    , yByte <- [0..31]
    , let byte = Mem.read error mem (Addr (fromIntegral (0x2400 + x * 32 + yByte)))
    , yBit <- [0..7]
    , byte `testBit` yBit
    , let y  = 8 * yByte + yBit
    ]

reachFrame :: EmuState -> EmuState -> Bool
reachFrame s0 s1 = do
  let EmuState{ticks=ticks0} = s0
  let EmuState{ticks=ticks1} = s1
  let f0 = unTicks ticks0 `div` cyclesPerFrame
  let f1 = unTicks ticks1 `div` cyclesPerFrame
  let yes = f1 > f0
  yes
  where cyclesPerFrame = 33333
