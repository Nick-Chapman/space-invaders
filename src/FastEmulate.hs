
-- GOAL: first compile to residual program, and emulate that
-- For the moment, attempt to work to existing interface of Emulate, and see how we get on

module FastEmulate (
  Ticks(..),
  prettyPrefix,
  EmuState(..), initState,
  CB(..), emulate,
  Bit(..),
  ) where

import Addr (Addr)
import Buttons (Buttons)
import Byte (Byte(..))
import Compile (compileOp,compileAt)
import Cpu (Cpu,Reg(..),Flag(..))
import Data.Bits (complement,(.&.),(.|.),xor,shiftL,shiftR,testBit,setBit,clearBit)
import Data.Map (Map)
import HiLo (HiLo(..))
import InstructionSet (Instruction(..),Op(Op0),Op0(RST))
import Mem (Mem)
import Phase (Phase)
import Residual (Exp1(..),Exp8(..),Exp16(..),Exp17(..),Program(..),AVar)
import Rom (Rom)
import Shifter (Shifter)
import Sounds (soundControl)
import Text.Printf (printf)
import qualified Addr (toHiLo,fromHiLo,bump,addCarryOut)
import qualified Buttons
import qualified Byte (addWithCarry,toUnsigned)
import qualified Cpu (init,get,set,getFlag,setFlag)
import qualified Data.Map.Strict as Map (empty,findWithDefault,fromList,insert)
import qualified Mem (init,read,write)
import qualified Phase (Byte,Addr,Bit)
import qualified Rom (size)
import qualified Shifter (Reg,init,set,get)
import qualified Sounds (Playing,initPlaying)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map (toList,fromListWith)
import Static (startPoints,oneStepReach,searchReach,returnAddresses)


-- dont think prettyPrefix should be here
prettyPrefix :: EmuState -> String -> String
prettyPrefix s message = do
  let pc = programCounter s
  unwords [ prettyTicks s , show pc , ":", message ]

prettyTicks :: EmuState -> String
prettyTicks EmuState{ticks,icount} =
  unwords [ printf "%8d" icount, rjust 11 (show ticks) ]

rjust :: Int -> String -> String
rjust n s = take (max 0 (n - length s)) (repeat ' ') <> s


newtype Ticks = Ticks { unTicks :: Int } deriving (Eq,Ord,Num)

instance Show Ticks where show = printf "[%d]" . unTicks

newtype Bit = Bit { unBit :: Bool }

instance Show Bit where show (Bit b) = if b then "1" else "0"

data FastEmuTime

instance Phase FastEmuTime where
  type Byte FastEmuTime = Byte
  type Addr FastEmuTime = Addr
  type Bit FastEmuTime = Bit

data EmuState = EmuState
  { ticks :: Ticks -- cycle count
  , icount :: Int -- instruction count
  , cpu :: Cpu FastEmuTime
  , shifter :: Shifter FastEmuTime
  , mem :: Mem
  , interrupts_enabled :: Bool
  , nextWakeup :: Ticks
  , playing :: Sounds.Playing
  , programs :: Map Addr Program
  , rstHalf :: Program
  , rstVblank :: Program
  }

instance Show EmuState where
  show EmuState{cpu=_cpu} =
    unwords [ show _cpu
            -- , "shifter(00,0000)"
            --, show shifter -- TODO: seems we have no tests which show non-zero shifter regs
            ]


goFaster :: Bool
goFaster = False

initState :: Rom -> IO EmuState
initState rom = do
  programs <- Map.fromList <$> if goFaster then fastPrograms rom else return (slowPrograms rom)
  return $ EmuState
    { ticks = 0
    , icount = 0
    , cpu = Cpu.init (Byte 0) (Bit False)
    , shifter = Shifter.init (Byte 0)
    , mem = Mem.init rom
    , interrupts_enabled = False
    , nextWakeup = halfFrameTicks
    , playing = Sounds.initPlaying
    , programs
    , rstHalf = compileOp rom (Op0 (RST 1))
    , rstVblank = compileOp rom (Op0 (RST 2))
    }


slowPrograms :: Rom -> [(Addr,Program)]
slowPrograms rom = do
  let
    programsForEveryAddress =
      [ (addr,program)
      | addr <- [0.. Rom.size rom - 1]
      , let program = compileAt (\_ -> False) rom addr ]

  programsForEveryAddress


fastPrograms :: Rom -> IO [(Addr,Program)]
fastPrograms rom = do
  let
    programsForEveryAddress = slowPrograms rom

    step :: Addr -> [Addr]
    step a = Map.findWithDefault (error $ "step: " <> show a) a stepMap
        where
          stepMap :: Map Addr [Addr]
          stepMap = Map.fromList [ (a, oneStepReach p) | (a,p) <- programsForEveryAddress ]

  reachSet <- searchReach step startPoints

  let
    joinPoints = [ b | (b,as) <- collate backward, length as > 1 ]
      where
        backward = [ (b,a) | a <- Set.toList reachSet, b <- step a ]

        collate :: Ord a => [(a,b)] -> [(a,[b])]
        collate pairs = Map.toList $ Map.fromListWith (++) [ (a,[b]) | (a,b) <- pairs ]

    returnPoints =
      [ r
      | (a,p) <- programsForEveryAddress
      , a `elem` reachSet
      , r <- returnAddresses p
      ]

    inlinedSharingJoins =
      [ (addr,program)
      | addr <- Set.toList labels
      , let program = compileAt (`notElem` labels) rom addr ]
      where
        labels = Set.fromList (startPoints ++ returnPoints ++ joinPoints)

  return inlinedSharingJoins



data CB = CB
  { traceI :: Maybe (EmuState -> Instruction Byte -> IO ())
  }

emulate :: CB -> Buttons -> EmuState -> IO EmuState
emulate cb buttons pre@EmuState{programs} = do
  let EmuState{rstHalf,rstVblank} = pre
  let (pre1,program) =
        case checkI pre of
          (pre1,Nothing) -> do
            let pc = programCounter pre
            (pre1,Map.findWithDefault (error $ "no program at pc: " <> show pc) pc programs)
          (pre1,Just half) -> do
            (pre1,if half then rstHalf else rstVblank)
  let env = emptyEnv buttons
  post@EmuState{} <- emulateProgram cb env pre1 program
  return post


checkI :: EmuState -> (EmuState,Maybe Bool)
checkI s@EmuState{interrupts_enabled} = do
  case timeToWakeup s of
    Just s
      | interrupts_enabled -> do
          let s2 = s { interrupts_enabled = False }
          (s2,Just (halfFrame s2))
      | otherwise ->
        (s,Nothing)
    Nothing ->
      (s,Nothing)


getLitInstruction :: Instruction Exp8 -> Instruction Byte
getLitInstruction = \case
  Ins0 op -> Ins0 op
  Ins1 op b -> Ins1 op (getLit b)
  Ins2 op b1 b2 -> Ins2 op (getLit b1) (getLit b2)

getLit :: Exp8 -> Byte
getLit = \case
  E8_Lit b -> b
  x -> error $ "getLit: " <> show x

emulateProgram :: CB -> Env -> EmuState -> Program -> IO EmuState
emulateProgram CB{traceI} env s = emu env s
  where -- (s: read, u: write) -- TODO: eliminate double env using temp-vars during comp
    emu :: Env -> EmuState -> Program -> IO EmuState
    emu q u@EmuState{mem,playing} = \case
      S_AtRef _ p -> emu q u p
      S_MarkReturnAddress _ p -> emu q u p
      S_TraceInstruction i p -> do
        case traceI of
          Nothing -> return ()
          Just tr -> tr s (getLitInstruction i) -- NOTE: using s here
        emu q u p
      S_Advance n p -> emu q (advance n u) p
      S_Jump a -> return $ setPC (eval16 q s a) u
      S_If c p1 p2 -> if (eval1 q s c) then emu q u p1 else emu q u p2
      S_AssignReg r e p -> emu q (setReg r (eval8 q s e) u) p
      S_AssignFlag f e p -> emu q (setFlag f (eval1 q s e) u) p
      S_AssignShifterReg r e p -> emu q (setShifterReg r (eval8 q s e) u) p
      S_MemWrite a e p -> emu q u { mem = Mem.write mem (eval16 q s a) (eval8 q s e) } p
      S_Let17 v x p -> emu (insert17 q v (eval17 q s x)) u p
      S_Let16 v a p -> emu (insert16 q v (eval16 q s a)) u p
      S_Let8 v b p -> emu (insert8 q v (eval8 q s b)) u p
      S_SoundControl sound c p -> do
        emu q u { playing = soundControl (eval1 q s c) playing sound} p
      S_EnableInterrupts p -> emu q u { interrupts_enabled = True } p
      S_DisableInterrupts p -> emu q u { interrupts_enabled = False } p
      S_UnknownOutput n _ -> error $ "emulateProgram, unknown output: " ++ show n


advance :: Int -> EmuState -> EmuState
advance n s@EmuState{ticks,icount} =
  s { ticks = ticks + Ticks n, icount = icount + 1 }

timeToWakeup :: EmuState -> Maybe EmuState
timeToWakeup s@EmuState{ticks,nextWakeup} = do
  if ticks < nextWakeup
    then Nothing
    else do
    Just s { nextWakeup =
             Ticks ((unTicks ticks `div` unTicks halfFrameTicks) + 1) * halfFrameTicks
           }

halfFrameTicks :: Ticks
halfFrameTicks = Ticks (2000000 `div` 120) - n -- Experiment with reducing this value.
  -- turns out that even a reduction of just 1000 cycles
  -- will cause the game to hang after the "P" of "PLAY" is displayed
  where n = 0 --1000

halfFrame :: EmuState -> Bool
halfFrame EmuState{ticks} = do
  (unTicks ticks `mod` unTicks halfFrameTicks) `mod` 2 == 1


data V17 = V17 { hi :: Bit, dropHi :: Addr }

eval1 :: Env -> EmuState -> Exp1 -> Bool
eval1 q@Env{buttons} s@EmuState{cpu} = \case
  E1_Flag flag -> unBit (Cpu.getFlag cpu flag)
  E1_True -> True
  E1_False -> False
  E1_Flip c -> not (eval1 q s c)
  E1_IsZero b -> eval8 q s b == 0
  E1_TestBit b i -> eval8 q s b `testBit` i
  E1_HiBitOf17 x -> let V17{hi = Bit hi} = eval17 q s x in hi
  E1_IsParity b -> parity (eval8 q s b)
  E1_OrBit c1 c2 -> eval1 q s c1 || eval1 q s c2
  E1_AndBit c1 c2 -> eval1 q s c1 && eval1 q s c2
  E1_Button but -> Buttons.get but buttons

-- copied
parity :: Byte -> Bool
parity byte = length [ () | i <- [0..7], byte `testBit` i ] `mod` 2 == 0

eval8 :: Env -> EmuState -> Exp8 -> Byte
eval8 q s@EmuState{cpu,shifter,mem} = \case
  E8_Lit byte -> byte
  E8_Var v -> look8 q v
  E8_Reg r -> Cpu.get cpu r
  E8_ShifterReg r -> Shifter.get shifter r
  E8_ReadMem a -> Mem.read mem (eval16 q s a)
  -- TODO: we need more let-bindings to avoid repeated eval of same address
  -- when both hi/lo are used
  -- but it is ok if we are sure that the address(expression) is atomic
  E8_Hi a -> let HiLo{hi} = Addr.toHiLo (eval16 q s a) in hi
  E8_Lo a -> let HiLo{lo} = Addr.toHiLo (eval16 q s a) in lo
  E8_AndB b1 b2 -> (.&.) (eval8 q s b1) (eval8 q s b2)
  E8_OrB b1 b2 -> (.|.) (eval8 q s b1) (eval8 q s b2)
  E8_XorB b1 b2 -> xor (eval8 q s b1) (eval8 q s b2)
  E8_Complement b -> complement (eval8 q s b)
  E8_ShiftLeft b off -> eval8 q s b `shiftL` (Byte.toUnsigned (eval8 q s off))
  E8_ShiftRight b off -> eval8 q s b `shiftR` (Byte.toUnsigned (eval8 q s off))
  E8_Ite c b1 b2 -> if (eval1 q s c) then eval8 q s b1 else eval8 q s b2
  E8_UpdateBit b i c -> (if eval1 q s c then setBit else clearBit) (eval8 q s b) i
  E8_UnknownInput x -> error $ "eval8: unknown input" <> show x

eval16 :: Env -> EmuState -> Exp16 -> Addr
eval16 q s = \case
  E16_Lit a -> a
  E16_Var v -> look16 q v
  E16_HiLo HiLo{hi,lo} -> Addr.fromHiLo HiLo {hi = eval8 q s hi, lo = eval8 q s lo}
  E16_OffsetAdr i a -> Addr.bump (eval16 q s a) i
  E16_AddWithCarry cin b1 b2 -> do
    let (res,cout) = Byte.addWithCarry (eval1 q s cin) (eval8 q s b1) (eval8 q s b2)
    Addr.fromHiLo HiLo { hi = if cout then 1 else 0, lo = res }
  E16_DropHiBitOf17 x -> do
    let V17{dropHi=res} = eval17 q s x
    res

eval17 :: Env -> EmuState -> Exp17 -> V17
eval17 q s = \case
  E17_Var v -> look17 q v
  E17_Add a1 a2 -> do
    let (dropHi, hi) = Addr.addCarryOut (eval16 q s a1) (eval16 q s a2)
    V17 { hi = Bit hi, dropHi }

setPC :: Addr -> EmuState -> EmuState
setPC addr = do
  let HiLo{hi,lo} = Addr.toHiLo addr
  setReg PCH hi . setReg PCL lo

setShifterReg :: Shifter.Reg -> Byte -> EmuState -> EmuState
setShifterReg reg byte state@EmuState{shifter} = do
  state { shifter = Shifter.set shifter reg byte }

setReg :: Reg -> Byte -> EmuState -> EmuState
setReg reg byte state@EmuState{cpu} = do
  state { cpu = Cpu.set cpu reg byte }

setFlag :: Flag -> Bool -> EmuState -> EmuState
setFlag flag bool state@EmuState{cpu} = do
  state { cpu = Cpu.setFlag cpu flag (Bit bool) }

programCounter :: EmuState -> Addr
programCounter EmuState{cpu} = do
  let lo = Cpu.get cpu PCL
  let hi = Cpu.get cpu PCH
  Addr.fromHiLo HiLo{hi,lo}


data Env = Env
  { m17 :: Map AVar V17
  , m16 :: Map AVar Addr
  , m8 :: Map AVar Byte
  , buttons :: Buttons
  }

emptyEnv :: Buttons -> Env
emptyEnv buttons = Env
  { m17 = Map.empty
  , m16 = Map.empty
  , m8 = Map.empty
  , buttons
  }

look8 :: Env -> AVar -> Byte
look8 Env{m8} v = Map.findWithDefault (error $ "eval8: " <> show v) v m8

insert8 :: Env -> AVar -> Byte -> Env
insert8 env@Env{m8} v a = env { m8 = Map.insert v a m8 }

look16 :: Env -> AVar -> Addr
look16 Env{m16} v = Map.findWithDefault (error $ "eval16: " <> show v) v m16

insert16 :: Env -> AVar -> Addr -> Env
insert16 env@Env{m16} v a = env { m16 = Map.insert v a m16 }

look17 :: Env -> AVar -> V17
look17 Env{m17} v = Map.findWithDefault (error $ "eval17: " <> show v) v m17

insert17 :: Env -> AVar -> V17 -> Env
insert17 env@Env{m17} v a = env { m17 = Map.insert v a m17 }
