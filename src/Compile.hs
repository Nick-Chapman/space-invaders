
module Compile (opPrograms,compileAt) where

import Addr (Addr)
import Byte (Byte(..))
import Control.Monad (ap,liftM)
import Cpu (Cpu(..),Reg(..),Flag(..))
import Data.Set (Set)
import Effect (Eff)
import HiLo (HiLo(..))
import InstructionSet (Instruction,Op(..),Op1(..),decode,encode)
import Rom (Rom)
import Phase (Phase)
import Residual (Exp1(..),Exp8(..),Exp16(..),Exp17(..),Program(..),AVar(..))
import Semantics (InterruptHandling(..))
import Shifter (Shifter(..))
import qualified Addr as Addr (toHiLo,fromHiLo,bump)
import qualified Cpu (get,set,getFlag,setFlag)
import qualified Data.Set as Set
import qualified Effect as E (Eff(..))
import qualified Rom (lookup)
import qualified Phase (Bit,Byte,Addr)
import qualified Semantics (fetchDecodeExec,decodeExec,Conf(..))
import qualified Shifter (Reg(..),get,set,allRegs)


data CompTime

instance Phase CompTime where
  type Bit CompTime = Exp1
  type Byte CompTime = Exp8
  type Addr CompTime = Exp16


opPrograms :: Rom -> [(Op,Program)]
opPrograms rom = do
  let skipOps = [Op1 IN, Op1 OUT]
  let ops = [ op | b <- [0..0xFF], let op = decode b, op `notElem` skipOps ]
  let cpu = initCpu HiLo {hi = E8_Reg PCH, lo = E8_Reg PCL }
  let state = initState rom cpu  -- TODO: shouldn't need rom here
  [
    (op,program)
    | op <- ops
    , let semantics = Semantics.decodeExec (E8_Lit (InstructionSet.encode op))
    , let program = runGen $ compileThen semantics state (return . programFromState)
    ]


compileAt :: (Addr -> Bool) -> Rom -> Addr -> Program
compileAt inline rom addr = do
  let cpu = initCpu HiLo {hi = pch, lo = pcl }
        where
          HiLo{hi,lo} = Addr.toHiLo addr
          pch = E8_Lit hi
          pcl = E8_Lit lo
  let state = initState rom cpu
  let visited :: Visited = Set.insert addr Set.empty
  runGen $ compileFrom inline visited state


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


type Visited = Set Addr

type CompileRes = Gen Program

semConf :: Semantics.Conf
semConf = Semantics.Conf { interruptHandling = IgnoreInterrupts }

compileFrom :: (Addr -> Bool) -> Visited -> State -> CompileRes
compileFrom inline = go
  where

    theSemantics = Semantics.fetchDecodeExec semConf

    -- Tracking visited (for loop breaking) is not needed when we compile w.r.t join-points
    go :: Visited -> State -> CompileRes
    go visited state =
      compileThen theSemantics state $ \state@State{cpu} -> do
      case getConcreteAddrMaybe (getPC cpu) of
        Nothing ->
          return (programFromState state)
        Just pc -> do
          if pcInRom pc && pc `notElem` visited && inline pc
          then
            S_AtRef pc <$> go (Set.insert pc visited) state
          else
            return (programFromState state)


getPC :: Cpu CompTime -> Exp16
getPC cpu = E16_HiLo HiLo{hi,lo} where
  hi = Cpu.get cpu PCH
  lo = Cpu.get cpu PCL

pcInRom :: Addr -> Bool
pcInRom a = a < 0x2000


-- remove unchangeable part (rom etc) from the state
data State = State
  { cpu :: Cpu CompTime
  , shifter :: Shifter CompTime
  , rom :: Rom
  }

initState :: Rom -> Cpu CompTime -> State
initState rom cpu = State
  { cpu
  , shifter = initShifter
  , rom
  }

initShifter :: Shifter CompTime
initShifter = Shifter
  { hi = E8_ShifterReg Shifter.HI
  , lo = E8_ShifterReg Shifter.LO
  , off = E8_ShifterReg Shifter.OFF
  }

programFromState :: State -> Program
programFromState State{cpu,shifter} = do
  sequence (regUpdates ++ flagUpdates ++ shifterUpdates) finish
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
      shifterUpdates =
        [ S_AssignShifterReg reg v
        | reg <- Shifter.allRegs
        , let v = Shifter.get shifter reg
        , not (v == E8_ShifterReg reg)
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



compileThen :: Eff CompTime (Instruction Exp8, Int) -> State -> (State -> CompileRes) -> CompileRes
compileThen semantics state k =
  run state semantics $ \state (_i,_n) -> k state
  where

    run :: State -> Eff CompTime a -> (State -> a -> CompileRes) -> CompileRes
    run s@State{cpu,shifter,rom} eff k = case eff of

      E.Ret x -> k s x
      E.Bind eff f -> run s eff $ \s a -> run s (f a) k

      E.GetReg r -> k s (Cpu.get cpu r)
      E.SetReg r b -> k s { cpu = Cpu.set cpu r b } ()
      E.GetFlag flag -> k s (Cpu.getFlag cpu flag)
      E.SetFlag flag v -> k s { cpu = Cpu.setFlag cpu flag v } ()

      E.ReadMem a ->
        case tryRomLookupE16 rom a of
          Just byte -> k s (E8_Lit byte)
          Nothing -> k s (E8_ReadMem a)
      E.WriteMem a b -> S_MemWrite a b <$> k s ()

      E.GetShifterReg r -> k s (Shifter.get shifter r)
      E.SetShifterReg r b -> k s { shifter = Shifter.set shifter r b } ()

      E.EnableInterrupts -> S_EnableInterrupts <$> k s ()
      E.DisableInterrupts -> S_DisableInterrupts <$> k s ()

      E.AreInterruptsEnabled -> k s E1_InterruptsEnabled

      E.TimeToWakeup -> k s E1_TimeToWakeup

      E.GetInterruptInstruction -> do
        t <- k s (E8_Lit 0xCF)
        e <- k s (E8_Lit 0xD7)
        return $ S_If E1_HalfFrame t e

      {-E.GetInterruptInstruction -> do
        k s (E8_Ite E1_HalfFrame (E8_Lit 0xCF) (E8_Lit 0xD7))-} -- causes non-literal decode

      E.Decode e ->
        case e of
          E8_Lit byte -> k s (decode byte)
          _ -> error $ "Decode, non-literal: " <> show e
      E.MarkReturnAddress a -> S_MarkReturnAddress a <$> k s ()

      E.MakeBit bool -> k s (if bool then E1_True else E1_False)
      E.Flip e -> k s (E1_Flip e)
      E.AndBit b1 b2 -> k s (E1_AndBit b1 b2)
      E.OrBit b1 b2 -> k s (E1_OrBit b1 b2)
      E.CaseBit i -> do
        t <- k s True
        e <- k s False
        return $ S_If i t e

      E.MakeByte w8 -> k s (E8_Lit (Byte w8))
      E.ShiftRight byte offset -> k s (E8_ShiftRight byte offset)
      E.ShiftLeft byte offset -> k s (E8_ShiftLeft byte offset)
      E.Complement e -> k s (E8_Complement e)
      E.AndB e1 e2 -> share8 (k s) (E8_AndB e1 e2)
      E.OrB e1 e2 -> share8 (k s) (E8_OrB e1 e2)
      E.XorB e1 e2 -> share8 (k s) (E8_XorB e1 e2)
      E.Ite i t e -> k s (E8_Ite i t e)
      E.AddWithCarry cin v1 v2 -> do
        tmp <- NewAVar
        let v = E8_Lo (E16_Var tmp)
        let cout = E1_TestBit (E8_Hi (E16_Var tmp)) 0
        S_Let16 tmp (E16_AddWithCarry cin v1 v2) <$> k s (v,cout)
      E.IsSigned e -> k s (E1_TestBit e 7)
      E.IsZero e -> k s (E1_IsZero e)
      E.IsParity e -> k s (E1_IsParity e)
      E.TestBit e i -> k s (E1_TestBit e i)
      E.UpdateBit e i p -> k s (E8_UpdateBit e i p)
      E.CaseByte e ->
        case e of
          E8_Lit (Byte w8) -> k s w8
          _ -> error $ "CaseByte, non-literal " ++ show e

      E.MakeAddr hilo -> k s (make_E16_HiLo hilo)
      E.SplitAddr a ->
        case a of
          E16_HiLo hilo -> k s hilo
          _ -> do
            let hi = make_E8_Hi a
            let lo = make_E8_Lo a
            k s HiLo {hi,lo}
      E.OffsetAddr n a ->
        case a of
          E16_Lit a -> k s (E16_Lit (Addr.bump a n))
          _ -> do
            v <- NewAVar
            S_Let16 v (E16_OffsetAdr n a) <$> k s (E16_Var v)
      E.Add16 a1 a2 -> do
        var <- NewAVar
        let res = E16_DropHiBitOf17 (E17_Var var)
        let cout = E1_HiBitOf17 (E17_Var var)
        body <- k s (res,cout)
        return $ S_Let17 var (E17_Add a1 a2) body

      E.UnknownInput port -> k s (E8_UnknownInput port)
      E.UnknownOutput port -> S_UnknownOutput port <$> k s ()
      E.GetButton but -> k s (E1_Button but)
      E.SoundControl sound p -> S_SoundControl sound p <$> k s ()



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


tryRomLookupE16 :: Rom -> Exp16 -> Maybe Byte
tryRomLookupE16 rom exp16 = case getConcreteAddrMaybe exp16 of
  Nothing -> Nothing
  Just a -> Rom.lookup rom a

getConcreteAddrMaybe :: Exp16 -> Maybe Addr
getConcreteAddrMaybe = \case
  E16_HiLo (HiLo {lo=E8_Lit lo,hi = E8_Lit hi}) -> Just $ Addr.fromHiLo HiLo{hi,lo}
  E16_Lit a -> Just a
  _ -> Nothing

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
