
module Static (ops,retarget) where

import Prelude hiding (lines)

import Control.Monad (ap,liftM,forM_)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set

import Addr (Addr)
import Buttons (buttons0)
import Byte (Byte(..))
import Cpu (Cpu(..),Reg(..),Flag(..))
import Data.Word8 (Word8)
import Effect (Eff)
import HiLo (HiLo(..))
import InstructionSet (Op1(..),Op(..),encode,decode)
import Phase (Phase)
import Rom2k (Rom)
import Sounds (Sound)
import qualified Addr as Addr (toHiLo,fromHiLo,toUnsigned,bump)
import qualified Cpu (get,set,getFlag,setFlag)
import qualified Effect as E (Eff(..))
import qualified Phase (Bit,Byte,Addr,Ticks)
import qualified Rom2k (load,read,size)
import qualified Semantics (exploreDecodeExec,exploreFetchDecodeExec)


ops :: IO ()
ops = do
  putStrLn "*static-ops*"
  roms <- loadRoms
  let skipOps = [Op1 IN, Op1 OUT]
  let ops =
        [ op | b <- [0..0xFF], let op = decode b ]
        \\ skipOps
  let cpu = initCpu HiLo {hi = E8_Reg PCH, lo = E8_Reg PCL }
  let state = initState roms cpu
  forM_ ops $ \op -> do
    let semantics = Semantics.exploreDecodeExec (E8_Lit (InstructionSet.encode op))
    let program = runGen $
          compileThen semantics state (return . programFromState)
    print $ vert [ lay (show (encode op) ++ " --> " ++ show op), lay ""
                 , tab (layProgram program) ]


retarget :: Bool -> IO ()
retarget inline = do
  putStrLn "*static-retarget*"
  roms <- loadRoms
  --let _as = [0, 0x1A32, 0x18DC , 0x1A7F]
  let addrs =
        if inline
        then [0x20] -- explore inlining for some addresses
        else [0..0x1FFF] -- no inlining.. lets see every rom address
  forM_ addrs $ \addr -> do
    let program = compileAt inline roms addr
    print $ vert [ lay (show addr ++ ":"), tab (layProgram program) ]


data Roms = Roms
  { e :: Rom
  , f :: Rom
  , g :: Rom
  , h :: Rom
  }

loadRoms :: IO Roms
loadRoms = do
  e <- Rom2k.load "roms/invaders.e"
  f <- Rom2k.load "roms/invaders.f"
  g <- Rom2k.load "roms/invaders.g"
  h <- Rom2k.load "roms/invaders.h"
  return $ Roms {e,f,g,h}


type Semantics = Eff CompTime ()

data CompTime

instance Phase CompTime where
  type Bit CompTime = Exp1
  type Byte CompTime = Exp8
  type Addr CompTime = Exp16
  type Ticks CompTime = ExpTicks

data ExpTicks -- TODO


initCpu :: HiLo Exp8 -> Cpu CompTime
initCpu HiLo{hi=pch,lo=pcl} = do
  Cpu { pch, pcl
      , sph = e SPH, spl = e SPL
      , regA = e A, regB = e B, regC = e C, regD = e D
      , regE = e E, regH = e H, regL = e L
      , flagS = b FlagS, flagZ = b FlagZ, flagA = b FlagA
      , flagP = b FlagP, flagCY = b FlagCY
      }
  where
    e = E8_Reg
    b = E1_Flag



-- remove unchangeable part (roms etc) from the state
data State = State
  { interruptsEnabled :: Exp1
  , cpu :: Cpu CompTime
  , roms :: Roms
  }

initState :: Roms -> Cpu CompTime -> State
initState roms cpu = State
  { interruptsEnabled = E1_False
  , cpu
  , roms
  }

programFromState :: State -> Program
programFromState State{cpu} = do
  -- TODO: show interruptsEnabled... check with output of DI
  sequence (regUpdates ++ flagUpdates) finish
    where
      regUpdates =
        [ S_AssignReg reg v
        | reg <- allRegs
        , let v = Cpu.get cpu reg
        , not (v == E8_Reg reg)
        ]
      flagUpdates =
        [ S_AssignFlag flag v
        | flag <- allFlags
        , let v = Cpu.getFlag cpu flag
        , not (v == E1_Flag flag)
        ]

      allRegs = [A,B,C,D,E,H,L
                -- ,PCH,PCL
                ,SPH,SPL]
      allFlags = [FlagS,FlagZ,FlagA,FlagP,FlagCY]

      sequence xs = foldr (.) id xs

      finish = S_Jump a
        where
          a = make_E16_HiLo HiLo{hi = pch, lo = pcl}
          pch = Cpu.get cpu PCH
          pcl = Cpu.get cpu PCL



tryRomLookup :: Roms -> Exp16 -> Maybe Byte
tryRomLookup Roms{e,f,g,h} exp16 = case getConcreteAddrMaybe exp16 of
  Nothing -> Nothing
  Just a -> do
    let i :: Int = Addr.toUnsigned a
    if
      | i < k2 -> Just (Rom2k.read h i)
      | i < k4 -> Just (Rom2k.read g (i - k2))
      | i < k6 -> Just (Rom2k.read f (i - k4))
      | i < k8 -> Just (Rom2k.read e (i - k6))
      | otherwise -> Nothing
      where
        k2 = Rom2k.size
        k4 = Rom2k.size * 2
        k6 = Rom2k.size * 3
        k8 = Rom2k.size * 4


type CompileRes = Gen Program

getPC :: Cpu CompTime -> Exp16
getPC cpu = E16_HiLo HiLo{hi,lo} where
  hi = Cpu.get cpu PCH
  lo = Cpu.get cpu PCL


type Visited = Set Addr


compileAt :: Bool -> Roms -> Addr -> Program
compileAt inline roms addr = do
  let cpu = initCpu HiLo {hi = pch, lo = pcl }
        where
          HiLo{hi,lo} = Addr.toHiLo addr
          pch = E8_Lit hi
          pcl = E8_Lit lo
  let state = initState roms cpu
  let visited :: Visited = Set.insert addr Set.empty
  runGen $ compileFrom inline visited state

compileFrom :: Bool -> Visited -> State -> CompileRes
compileFrom inline = go
  where

    theSemantics = Semantics.exploreFetchDecodeExec

    go :: Visited -> State -> CompileRes
    go visited state =
      compileThen theSemantics state $ \state@State{cpu} -> do
      case getConcreteAddrMaybe (getPC cpu) of
        Nothing ->
          return (programFromState state)
        Just pc -> do
          if pcInRom pc && pc `notElem` visited && inline
          then
            S_AtRef pc <$> go (Set.insert pc visited) state
          else
            return (programFromState state)


pcInRom :: Addr -> Bool
pcInRom a = a < 0x2000

compileThen :: Semantics -> State -> (State -> CompileRes) -> CompileRes
compileThen semantics state k =
  run state semantics $ \state () -> k state
  where

    run :: State -> Eff CompTime a -> (State -> a -> CompileRes) -> CompileRes
    run s@State{cpu,roms} eff k = case eff of
      E.Ret x -> k s x
      E.Bind eff f -> run s eff $ \s a -> run s (f a) k

      E.GetReg r -> do
        k s (Cpu.get cpu r)

      E.SetReg r b -> do
        k s { cpu = Cpu.set cpu r b } ()

      E.ReadMem a -> do
        case tryRomLookup roms a of
          Just byte -> do
            k s (E8_Lit byte)
          Nothing -> do
            k s (E8_ReadMem a)

      E.WriteMem a b -> do
        after <- k s ()
        return $ S_MemWrite a b after

      E.SplitAddr a -> do
        case a of
          E16_HiLo hilo -> k s hilo
          _ -> do
            let hi = make_E8_Hi a
            let lo = make_E8_Lo a
            k s HiLo {hi,lo}

      E.MakeAddr hilo -> do
        k s (make_E16_HiLo hilo)

      E.OffsetAddr n a ->
        case a of
          E16_Lit a -> k s (E16_Lit (Addr.bump a n))
          _ -> do
            v <- NewAVar
            after <- k s (E16_Var v)
            return $ S_Let16 v (E16_OffsetAdr n a) after

      E.Decode e -> do
        case e of
          E8_Lit byte -> k s (decode byte)
          _ -> error $ "decode, non literal: " <> show e

      E.MakeByte w8 -> k s (E8_Lit (Byte w8))

      E.AddWithCarry cin v1 v2 -> do
        tmp <- NewAVar
        let v = E8_Lo (E16_Var tmp)
        let cout = E1_TestBit (E8_Hi (E16_Var tmp)) 0
        after <- k s (v, cout)
        return $ S_Let16 tmp (E16_AddWithCarry cin v1 v2) after

      E.Complement e -> k s (E8_Complement e)
      E.Flip e -> k s (E1_Flip e)

      E.AndB e1 e2 -> do
        share8 (k s) (E8_AndB e1 e2)

      E.OrB e1 e2 -> do
        share8 (k s) (E8_OrB e1 e2)

      E.XorB e1 e2 -> do
        share8 (k s) (E8_XorB e1 e2)

      E.Add16 a1 a2 -> do
        var <- NewAVar
        let res = E16_DropHiBitOf17 (E17_Var var)
        let cout = E1_HiBitOf17 (E17_Var var)
        body <- k s (res,cout)
        return $ S_Let17 var (E17_Add a1 a2) body

      E.GetFlag flag -> do
        k s (Cpu.getFlag cpu flag)

      E.SetFlag flag v -> do
        k s { cpu = Cpu.setFlag cpu flag v } ()

      E.IsSigned e -> k s (E1_TestBit e 7)
      E.IsZero e -> k s (E1_IsZero e)
      E.IsParity e -> k s (E1_IsParity e)

      E.CaseBit i -> do
        t <- k s True
        e <- k s False
        return $ S_If i t e

      E.MakeBit bool -> k s (if bool then E1_True else E1_False)

      E.RotateRight e -> k s (E8_RotR e)
      E.RotateLeft e -> k s (E8_RotL e)

      E.ShiftRight byte offset -> k s (E8_ShiftRight byte offset)
      E.ShiftLeft byte offset -> k s (E8_ShiftLeft byte offset)
      E.Ite i t e -> k s (E8_Ite i t e)

      E.AndBit b1 b2 -> k s (E1_AndBit b1 b2)
      E.OrBit b1 b2 -> k s (E1_OrBit b1 b2)


      E.EnableInterrupts -> k s { interruptsEnabled = E1_True } ()
      E.DisableInterrupts -> k s { interruptsEnabled = E1_False } ()

      E.AreInterruptsEnabled -> do
        -- look in state??
        t <- k s True
        e <- k s False
        return $ S_If E1_InterruptsEnabled t e

      E.TimeToWakeup -> do
        t <- k s True
        e <- k s False
        return $ S_If E1_TimeToWakeup t e

      E.GetInterruptInstruction -> do
        t <- k s (E8_Lit 0xCF)
        e <- k s (E8_Lit 0xD7)
        return $ S_If E1_HalfFrame t e

      E.UnknownInput port -> do
        k s (E8_UnknownInput port)

      E.UnknownOutput port -> do
        after <- k s ()
        return $ S_UnknownOutput port after

      E.GetButtons{} -> do
        -- Not right. Need a symbolic representation for the current button state.
        k s buttons0

      E.DispatchByte e -> do
        case e of
          E8_Lit (Byte w8) -> k s w8
          _ -> error $ "dispatch-byte, " ++ show e

      E.TestBit e i -> k s (E1_TestBit e i)
      E.UpdateBit e i p -> k s (E8_UpdateBit e i p)

      E.SoundControl sound p -> do
        after <- k s ()
        return $ S_SoundControl sound p after

      -- TODO: handle shift register akin to cpu regs

      E.FillShiftRegister e -> do
        after <- k s ()
        return $ S_FillShiftRegister e after

      E.SetShiftRegisterOffset e -> do
        after <- k s ()
        return $ S_SetShiftRegsterOffset e after

      E.GetShiftRegisterAtOffset -> do
        k s E8_GetShiftRegisterAtOffset


share8 :: (Exp8 -> Gen Program) -> (Exp8 -> Gen Program)
share8 k exp = do
  var <- NewAVar
  body <- k (E8_Var var)
  return $ S_Let8 var exp body


data Gen a where
  Ret :: a -> Gen a
  Bind :: Gen a -> (a -> Gen b) -> Gen b
  NewAVar :: Gen AVar

instance Functor Gen where fmap = liftM
instance Applicative Gen where pure = return; (<*>) = ap
instance Monad Gen where return = Ret; (>>=) = Bind

runGen :: Gen a -> a
runGen g = fst $ loop GenState{u=1} g where
  loop :: GenState -> Gen a -> (a,GenState)
  loop s = \case
    Ret x -> (x,s)
    Bind g f -> let (a,s') = loop s g in loop s' (f a)
    NewAVar -> let GenState{u} = s in (AVar u, GenState {u=u+1})

data GenState = GenState { u :: Int }

----------------------------------------------------------------------
-- generated programs

data Program
  = S_Stop
  | S_Jump Exp16
  | S_If Exp1 Program Program
  | S_AssignReg Reg Exp8 Program
  | S_AssignFlag Flag Exp1 Program
  | S_MemWrite Exp16 Exp8 Program
  | S_Let16 AVar Exp16 Program
  | S_Let8 AVar Exp8 Program
  | S_Let17 AVar Exp17 Program
  | S_FillShiftRegister Exp8 Program
  | S_SetShiftRegsterOffset Exp8 Program
  | S_AtRef Addr Program
  | S_UnknownOutput Word8 Program
  | S_SoundControl Sound Exp1 Program

data Exp17
  = E17_Add Exp16 Exp16
  | E17_Var AVar
  deriving (Eq)

data Exp1
  = E1_False
  | E1_True
  | E1_InterruptsEnabled
  | E1_TimeToWakeup
  | E1_HalfFrame
  | E1_Flag Flag
  | E1_TestBit Exp8 Int
  | E1_Flip Exp1
  | E1_AndBit Exp1 Exp1
  | E1_OrBit Exp1 Exp1
  | E1_IsZero Exp8
  | E1_IsParity Exp8
  | E1_HiBitOf17 Exp17
  deriving (Eq)

-- TODO: have Exp9, for result of 8-bit add-with-carry

data Exp8
  = E8_Lit Byte
  | E8_Reg Reg
  | E8_Hi Exp16
  | E8_Lo Exp16
  | E8_ReadMem Exp16
  | E8_RotL Exp8
  | E8_RotR Exp8
  | E8_UpdateBit Exp8 Int Exp1
  | E8_Complement Exp8
  | E8_AndB Exp8 Exp8
  | E8_OrB Exp8 Exp8
  | E8_XorB Exp8 Exp8
  | E8_ShiftRight Exp8 Exp8
  | E8_ShiftLeft Exp8 Exp8
  | E8_Ite Exp1 Exp8 Exp8
  | E8_Var AVar
  | E8_UnknownInput Word8
  | E8_GetShiftRegisterAtOffset
  deriving (Eq)

data Exp16
  = E16_HiLo (HiLo Exp8)
  | E16_OffsetAdr Int Exp16
  | E16_Var AVar
  | E16_AddWithCarry Exp1 Exp8 Exp8
  | E16_DropHiBitOf17 Exp17
  | E16_Lit Addr
  deriving (Eq)


getConcreteAddrMaybe :: Exp16 -> Maybe Addr
getConcreteAddrMaybe = \case
  E16_HiLo (HiLo {lo=E8_Lit lo,hi = E8_Lit hi}) -> Just $ Addr.fromHiLo HiLo{hi,lo}
  E16_Lit a -> Just a
  _ -> Nothing


make_E8_Hi :: Exp16 -> Exp8
make_E8_Hi = \case
  E16_Lit a -> let HiLo{hi} = Addr.toHiLo a in E8_Lit hi
  E16_HiLo HiLo{hi} -> hi
  a -> E8_Hi a

make_E8_Lo :: Exp16 -> Exp8
make_E8_Lo = \case
  E16_Lit a -> let HiLo{lo} = Addr.toHiLo a in E8_Lit lo
  E16_HiLo HiLo{lo} -> lo
  a -> E8_Lo a

make_E16_HiLo :: HiLo Exp8 -> Exp16
make_E16_HiLo = \case
  HiLo{hi=E8_Lit hi,lo = E8_Lit lo} -> do
    let a = Addr.fromHiLo HiLo {hi,lo}
    E16_Lit a

  HiLo{hi=E8_Hi a,lo = E8_Lo a'} | a==a' -> a
  hilo@HiLo{} -> E16_HiLo hilo


newtype AVar = AVar { u :: Int } -- address (16bit) variable
  deriving Eq

instance Show AVar where show AVar{u} = "a" ++ show u


instance Show Program where show = show . layProgram

layProgram :: Program -> Lay
layProgram = \case
  S_Stop -> lay "stop;"
  S_Jump a -> lay ("jump " ++ parenthesize (show a) ++ ";")
  S_If e s1 s2 ->
    vert [ lay ("if " ++ parenthesize (show e) ++ " {")
         , tab (layProgram s1)
         , lay "} else {"
         , tab (layProgram s2)
         , lay "}"
         ]
  S_AssignReg reg exp next ->
    vert [ lay (show reg ++ " := " ++ show exp ++ ";")
         , layProgram next
         ]
  S_AssignFlag flag exp1 next ->
    vert [ lay (show flag ++ " := " ++ show exp1 ++ ";")
         , layProgram next
         ]
  S_MemWrite lv rv next ->
    vert [ lay ("M[" ++ show lv ++ "] := " ++ show rv ++ ";")
         , layProgram next
         ]
  S_Let16 v e next ->
    vert [ lay ("let:16 " ++ show v ++ " = " ++ show e ++ " in")
         , layProgram next
         ]
  S_Let8 v e next ->
    vert [ lay ("let:8 " ++ show v ++ " = " ++ show e ++ " in")
         , layProgram next
         ]
  S_Let17 v e next ->
    vert [ lay ("let:17 " ++ show v ++ " = " ++ show e ++ " in")
         , layProgram next
         ]
  S_FillShiftRegister e next ->
    vert [ lay ("fill_shift_register" ++ parenthesize (show e) ++ ";")
         , layProgram next
         ]
  S_SetShiftRegsterOffset e next ->
    vert [ lay ("set_shift_register_offset" ++ parenthesize (show e) ++ ";")
         , layProgram next
         ]
  S_AtRef pc next ->
    vert [ lay ("#" ++ show pc)
         , layProgram next
         ]
  S_UnknownOutput port next ->
    vert [ lay ("unknown_output" ++ parenthesize (show port) ++ ";")
         , layProgram next
         ]
  S_SoundControl sound bool next ->
    vert [ lay ("sound_control" ++ show (sound,bool) ++ ";")
         , layProgram next
         ]

instance Show Exp1 where
  show = \case
    E1_False -> "false"
    E1_True -> "true"
    E1_InterruptsEnabled -> "g_interrupts_enabled"
    E1_TimeToWakeup -> "g_time_to_wakeup"
    E1_HalfFrame -> "g_half_frame"
    E1_Flag flag -> show flag
    E1_TestBit e i -> show e ++ "[" ++ show i ++ "]"
    E1_Flip p -> "!" ++ show p
    E1_AndBit e1 e2 -> parenthesize (show e1 ++ " && " ++ show e2)
    E1_OrBit e1 e2 -> parenthesize (show e1 ++ " || " ++ show e2)
    E1_IsZero e -> "is_zero" ++ parenthesize (show e)
    E1_IsParity e -> "parity" ++ parenthesize (show e)
    E1_HiBitOf17 e -> show e ++ "[16]"

instance Show Exp8 where
  show = \case
    E8_Lit x -> show x
    E8_Reg reg -> show reg
    E8_Hi a -> show a ++ "[15:8]"
    E8_Lo a -> show a ++ "[7:0]"
    E8_ReadMem a -> "M[" ++ show a ++ "]"
    E8_RotL e -> "rotateL" ++ parenthesize (show e)
    E8_RotR e -> "rotateR" ++ parenthesize (show e)
    E8_UpdateBit e i p -> "updateBit" ++ show (e,i,p)
    E8_Complement e -> "~" ++ show e
    E8_AndB e1 e2 -> parenthesize (show e1 ++ " & " ++ show e2)
    E8_OrB e1 e2 -> parenthesize (show e1 ++ " | " ++ show e2)
    E8_XorB e1 e2 -> parenthesize (show e1 ++ " ^ " ++ show e2)
    E8_ShiftRight e1 e2 -> parenthesize (show e1 ++ " >> " ++ show e2)
    E8_ShiftLeft e1 e2 -> parenthesize (show e1 ++ " << " ++ show e2)
    E8_Ite i t e -> parenthesize (show i ++ " ? " ++ show t ++ " : " ++ show e)
    E8_Var v -> show v
    E8_UnknownInput port -> "unknown_input" ++ parenthesize (show port)
    E8_GetShiftRegisterAtOffset -> "get_shift_register_at_offset()"

instance Show Exp16 where
  show = \case
    E16_HiLo HiLo{hi,lo} ->
      parenthesize (show hi ++ "," ++ show lo)
    E16_OffsetAdr n e ->
      parenthesize (show n ++ " + " ++ show e)
    E16_Var v ->
      show v
    E16_AddWithCarry cin e1 e2 ->
      "addWithCarry" ++ show (cin,e1,e2)
    E16_DropHiBitOf17 e ->
      show e ++ "[15:0]"
    E16_Lit x ->
      show x

instance Show Exp17 where
  show = \case
    E17_Add e1 e2 ->
      "add17" ++ show (e1,e2)
    E17_Var var ->
      show var

parenthesize :: String -> String
parenthesize x = "(" ++ x ++ ")"


data Lay = Lay { lines :: [String] }

instance Show Lay where show = Prelude.unlines . lines

lay :: String -> Lay
lay s = Lay { lines = [s] }

vert :: [Lay] -> Lay
vert = Lay . concat . map lines

tab :: Lay -> Lay
tab Lay{lines} = Lay $ [ "  " ++ line | line <- lines ]
