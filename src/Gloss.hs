
module Gloss where

import Data.Bits (testBit)
import Graphics.Gloss (scale,translate,Picture(Text),Point,pictures,color,white,red,polygon,black)
import Graphics.Gloss.Interface.IO.Game (Event(..),Key(..),SpecialKey(..),KeyState(..))
import System.IO (hFlush,stdout)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss (playIO,greyN,Display(InWindow),Event,Picture)

import Addr (Addr(..))
import Buttons (Buttons(..),buttons0)
import Emulate (Emulation(..),emulate,EmuState(..),Ticks(..))
import Mem (Mem,read)

data World = World
  { emulation :: Emulation
  , buttons :: Buttons
  , frameCount :: Int
  , disp :: Disp
  }

run :: Int -> Mem -> IO ()
run fps mem0 = do
  emulation <- Emulate.emulate mem0
  let EmuStep{post=EmuState{mem}} = emulation
  let model = World {emulation, buttons = buttons0, frameCount = 0, disp = getDisplayFromMem mem}
  let bgColour = if True then black else Gloss.greyN 0.3
  Gloss.playIO dis bgColour fps model
      (\  m -> do pic <- pictureWorld m; return $ doPosition pic)
      (\e m -> handleEventWorld e m)
      (\_ m -> updateWorld m)
  where
      sc = 3
      dis = Gloss.InWindow "Space Invaders" (sc * x,sc * y) (0,0)
      doPosition = doScale . doBorder . doTransOriginUL
      doScale = scale (float sc) (float sc)
      doBorder = translate (float border) (float border)
      doTransOriginUL = translate (-halfx) (-halfy)
      halfx = fromIntegral x / 2
      halfy = fromIntegral y / 2
      x = 256 + 2 * border
      y = 240 + 2 * border
      border = 20
      float :: Int -> Float
      float = fromIntegral

updateWorld :: World -> IO World
updateWorld World{buttons,emulation=e0,frameCount} = do
  putStr "."; flush
  loop e0
  where
    loop :: Emulation -> IO World
    loop = \case
      EmuStep {pre,post,continue} -> do
        case reachFrame pre post of
          False -> do
            emulation <- continue buttons
            loop emulation
          True -> do
            emulation <- continue buttons
            let EmuStep{post=EmuState{mem}} = emulation
            return $ World
              { buttons
              , emulation
              , frameCount = frameCount + 1
              , disp = getDisplayFromMem mem
              }

data Disp = Disp { onPixels :: [OnPixel] }

data OnPixel = OnPixel { x :: Int, y :: Int }

getDisplayFromMem :: Mem -> Disp
getDisplayFromMem mem = do
  Disp
    [ OnPixel {x, y}
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

handleEventWorld :: Gloss.Event -> World -> IO World
handleEventWorld event world@World{buttons} = do
  putStr "E"; flush
  return $ case event of
    EventKey (Char 'q') Down _ _ -> error "quit"
    EventKey (SpecialKey KeyDelete) Down _ _ -> world { buttons = buttons { coin = True } }
    EventKey (SpecialKey KeyDelete) Up _ _ -> world { buttons = buttons { coin = False } }
    EventKey (SpecialKey KeyTab) Down _ _ -> world { buttons = buttons { p1start = True } }
    EventKey (SpecialKey KeyTab) Up _ _ -> world { buttons = buttons { p1start = False } }
    EventKey (Char 'z') Down _ _ -> world { buttons = buttons { p1left = True } }
    EventKey (Char 'z') Up _ _ -> world { buttons = buttons { p1left = False } }
    EventKey (Char 'x') Down _ _ -> world { buttons = buttons { p1right = True } }
    EventKey (Char 'x') Up _ _ -> world { buttons = buttons { p1right = False } }
    EventKey (SpecialKey KeyEnter) Down _ _ -> world { buttons = buttons { p1shoot = True } }
    EventKey (SpecialKey KeyEnter) Up _ _ -> world { buttons = buttons { p1shoot = False } }
    _ -> world

pictureWorld :: World -> IO Gloss.Picture
pictureWorld World {frameCount,disp=disp@Disp{onPixels=_}} = do
  putStr "P"; flush
  return $ pictures
    [ pictureDisplay disp
    , translate 0 (-15) $ scale 0.1 0.1 $ picLabelled "frame" frameCount
    , translate 200 (-15) $ scale 0.1 0.1 $ picLabelled "secs" (frameCount `div` 60)
    ]

pictureDisplay :: Disp -> Picture
pictureDisplay Disp{onPixels} =
  pictures [ pixel ((fromIntegral x), (fromIntegral y)) | OnPixel{x,y} <- onPixels]

pixel :: Point -> Picture
pixel (x,y) = color white $ polygon [(x,y),(x,y+1),(x+1,y+1),(x+1,y)]

picLabelled :: Show a => String -> a -> Picture
picLabelled tag v =
  color red $ pictures [Text tag, translate 500 0 $ Text (show v) ]

flush :: IO ()
flush = hFlush stdout
