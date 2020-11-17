
module Static (main) where

import Addr (Addr)
import Compile (opPrograms,compileAt)
import Control.Monad (forM_,when)
import Data.List ((\\))
import Data.Map (Map)
import Data.Set (Set, union)
import InstructionSet (theDecodeTable)
import Residual (Program(..),Exp16(..),layOpPrograms,layPrograms)
import Semantics (InterruptHandling(IgnoreInterrupts))
import qualified Addr as Addr (toUnsigned)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Rom (loadInvaders)
import qualified Semantics (Conf(..))


main :: IO ()
main = do
  putStrLn "*static*"
  roms <- Rom.loadInvaders

  generateFile "0-decode-table" $ theDecodeTable

  generateFile "0-op-programs" $
    layOpPrograms (opPrograms roms)

  let
    semConf :: Semantics.Conf
    semConf = Semantics.Conf { interruptHandling = IgnoreInterrupts }

  let
    programsForEveryAddress =
      [ (addr,program)
      | addr <- [0..0x1FFF]
      , let program = compileAt semConf (\_ -> False) roms addr ]

  generateFile "1-programs-for-every-address" $
    layPrograms programsForEveryAddress

  let step :: Addr -> [Addr]
      step a = Map.findWithDefault (error $ "step: " <> show a) a stepMap
        where
          stepMap :: Map Addr [Addr]
          stepMap = Map.fromList [ (a, oneStepReach p) | (a,p) <- programsForEveryAddress ]

  reachSet <- searchReach step startPoints
  -- _printReachInfo reachSet

  let reachablePrograms = [ (a,p) | (a,p) <- programsForEveryAddress, a `elem` reachSet ]

  generateFile "2-reachable-programs" $
    layPrograms reachablePrograms

  let
    returnPoints =
      [ r
      | (a,p) <- programsForEveryAddress
      , a `elem` reachSet
      , r <- returnAddresses p
      ]

  let
    inlinedDeep =
      [ (addr,program)
      | addr <- Set.toList labels
      , let program = compileAt semConf (`notElem` labels) roms addr ]
      where
        labels = Set.fromList (startPoints ++ returnPoints)

  generateFile "3-inlined-deep" $
    layPrograms inlinedDeep

  let
    joinPoints = [ b | (b,as) <- collate backward, length as > 1 ]
      where
        backward = [ (b,a) | a <- Set.toList reachSet, b <- step a ]

        collate :: Ord a => [(a,b)] -> [(a,[b])]
        collate pairs = Map.toList $ Map.fromListWith (++) [ (a,[b]) | (a,b) <- pairs ]

  print ("#start",length startPoints)
  print ("#return",length returnPoints)
  print ("#join",length joinPoints)

  let
    inlinedSharingJoins =
      [ (addr,program)
      | addr <- Set.toList labels
      , let program = compileAt semConf (`notElem` labels) roms addr ]
      where
        labels = Set.fromList (startPoints ++ returnPoints ++ joinPoints)

  generateFile "4-inlined-upto-joins" $
    layPrograms inlinedSharingJoins

  return ()


generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  -- TODO: ensure "gen" exists
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)


startPoints :: [Addr]
startPoints =
  [ 0x00
  -- the interrupt addresss will be detected automatically when
  -- we compiler a version of the semantics which has the
  -- interrupt-logc made explicit.
  , 0x08, 0x10
  -- game object handlers, from annotated dissasembly, mem usage
  -- any hope to detect this automatically?
  , 0x028E -- obj0 (player) handler
  , 0x03BB -- obj1 (shot) handler
  , 0x0476 -- obj2 handler
  , 0x04B6 -- obj3 handler
  , 0x0682 -- obj4 handler
  -- Inspecting holes w.r.t annotated dissasembly, leads to more candidate start points:
  -- Address after indirect jump: "Run object's code (will return to next line)"
  , 0x026F
  -- And 3 more places:
  -- An interesting question is: How do we reach these address dynmaically?
  -- And it would be nice to check that we actually do!
  -- "Game task 4 when splash screen alien is shooting extra "C" with a squiggly shot"
  , 0x050E
  -- "SndOffExtPly:"
  , 0x17B4
  -- call to "One second delay"
  , 0x1834
  ]


searchReach :: (Addr -> [Addr]) -> [Addr] -> IO (Set Addr)
searchReach step initial = loop 0 Set.empty (Set.fromList initial)
  where
    loop :: Int -> Set Addr -> Set Addr -> IO (Set Addr)
    loop n acc frontierSet = do
      let frontier = Set.toList frontierSet
      when False $ do -- debug print disabled
        putStrLn $ unwords
          ["search, step:", show n,
          ", size(acc)=", show (Set.size acc),
          ", length(frontier)=", show (length frontier) ]
      -- Invariant: is the frontier in acc? NO
      -- TODO: disable this inv checker when confidence with this code grows
      when (not (all (`notElem` acc) frontier)) $ error "searchReach, inv failure"
      if frontier == [] then return acc else do
        let acc' = acc `union` Set.fromList frontier
        let frontier' = Set.fromList [ a2 | a1 <- frontier, a2 <- step a1, a2 `notElem` acc' ]
        loop (n+1) acc' frontier'


_printReachInfo :: Set Addr -> IO ()
_printReachInfo reachSet = do
  putStrLn $ "# reachable addresses = " <> show (Set.size reachSet)
  let xs = Set.toList reachSet
  -- This [0,1,2] is a bit of a hack.
  -- To do it properly would take account of the op-size;
  -- i.e. the number of following immediate bytes
  let ys = [ a | a0 <- xs, a <- [a0, a0+1, a0+2]]
  let allRom = [0..0x1FFF]
  let holes = allRom \\ ys
  let startHoles = holes \\ [ a+1 | a <- holes ]
  let endHoles = holes \\ [ a-1 | a <- holes ]
  let holeSpans =
        [ span
        | span@(_start,_end) <- zip startHoles endHoles
        -- , _end - _start > 2
        ]
  putStrLn "# holes..."
  forM_ holeSpans $ \(start,end) -> do
    putStrLn $ unwords
      ["# --", show start, ":", show end
      , ", size =", show (1 + Addr.toUnsigned (end - start))
      ]
  putStrLn ""


returnAddresses :: Program -> [Addr]
returnAddresses = \case
  S_MarkReturnAddress (E16_Lit a) _ -> [a]
  p -> [ a | p' <- afterProgram p, a <- returnAddresses p' ]

oneStepReach :: Program -> [Addr]
oneStepReach = \case
  S_Jump (E16_Lit a) -> [a]
  S_MarkReturnAddress (E16_Lit a) p -> a : oneStepReach p
  p -> [ a | p' <- afterProgram p, a <- oneStepReach p' ]

afterProgram :: Program -> [Program]
afterProgram = \case
  S_Jump{} -> []
  S_If _ p1 p2 -> [p1,p2]
  S_AssignReg _ _ p -> [p]
  S_AssignShifterReg _ _ p -> [p]
  S_AssignFlag _ _ p -> [p]
  S_MemWrite _ _ p -> [p]
  S_Let16 _ _ p -> [p]
  S_Let8 _ _ p -> [p]
  S_Let17 _ _ p -> [p]
  S_UnknownOutput _ p -> [p]
  S_SoundControl _ _ p -> [p]
  S_MarkReturnAddress _ p -> [p]
  S_TraceInstruction _ p -> [p]
  S_AtRef{} -> error "after, AtRef, not expected for non-inlined program"
  S_Advance _ p -> [p]
  S_EnableInterrupts p -> [p]
  S_DisableInterrupts p -> [p]
