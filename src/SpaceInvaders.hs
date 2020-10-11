
module SpaceInvaders(main) where

import Emulate (emulate)
import qualified Mem (init)
import qualified Rom2k (load)
import InstructionSet (prettyDecodeTable)

-- | Entry point to the Space Invaders emulation
main :: IO ()
main = do
  putStrLn "*space-invaders*"

  e <- Rom2k.load "roms/invaders.e"
  f <- Rom2k.load "roms/invaders.f"
  g <- Rom2k.load "roms/invaders.g"
  h <- Rom2k.load "roms/invaders.h"

  let _ = putStrLn (unlines prettyDecodeTable)

  let mem = Mem.init (e,f,g,h)
  emulate mem
