
module Compile (opPrograms,compileOp,compileInstruction,compileAt) where

import Addr (Addr)
import Byte (Byte(..))
import Control.Monad (ap,liftM)
import Cpu (Cpu(..),Reg16(..),Reg(..),Flag(..))
import Data.Set (Set)
import Effect (Eff)
import HiLo (HiLo(..))
import InstructionSet (Op(..),decode,encode,Instruction(..))
import Rom (Rom)
import Residual (CompTime,Exp1(..),Exp8(..),Exp16(..),Exp17(..),Program(..),AVar(..))
import Shifter (Shifter(..))
import qualified Addr as Addr (toHiLo,fromHiLo,bump)
import qualified Cpu (get16,set16,get,set,getFlag,setFlag)
import qualified Data.Set as Set
import qualified Effect as E (Eff(..))
import qualified Rom (lookup)
import qualified Semantics (fetchDecodeExec,decodeExec,execInstruction)
import qualified Shifter (Reg(..),get,set,allRegs)


opPrograms :: Rom -> [(Op,Program)]
opPrograms rom = do
  let ops = [ op | b <- [0..0xFF], let op = decode b ]
  [ (op, compileOp rom op) | op <- ops ]


compileOp :: Rom -> Op -> Program
compileOp rom op = do
  let cpu = initCpu HiLo {hi = E8_Reg PCH, lo = E8_Reg PCL }
  let state = initState rom cpu  -- TODO: shouldn't need rom here
  let semantics = Semantics.decodeExec (E8_Lit (InstructionSet.encode op))
  runGen $ compileThen semantics state (return . programFromState)


compileInstruction :: Rom -> Instruction Exp8 -> Program
compileInstruction rom instruction = do
  let cpu = initCpu HiLo {hi = E8_Reg PCH, lo = E8_Reg PCL }
  let state = initState rom cpu  -- TODO: shouldn't need rom here
  let semantics = Semantics.execInstruction instruction
  runGen $ compileThen semantics state (return . programFromState)


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
      , sp = a SP, hl = a HL
      , regA = e A, regB = e B, regC = e C, regD = e D, regE = e E
      , flagS = b FlagS, flagZ = b FlagZ, flagA = b FlagA
      , flagP = b FlagP, flagCY = b FlagCY
      }
  where
    a = E16_Reg
    e = E8_Reg
    b = E1_Flag


type Visited = Set Addr

type CompileRes = Gen Program

compileFrom :: (Addr -> Bool) -> Visited -> State -> CompileRes
compileFrom inline = go
  where

    theSemantics = Semantics.fetchDecodeExec

    -- Tracking visited (for loop breaking) is not needed when we compile w.r.t join-points
    go :: Visited -> State -> CompileRes
    go visited state = insertAtRef state $ do
      compileThen theSemantics state $ \state@State{cpu} -> do
      case getConcreteAddrMaybe (getPC cpu) of
        Nothing ->
          return (programFromState state)
        Just pc -> do
          if pcInRom pc && pc `notElem` visited && inline pc
          then
            go (Set.insert pc visited) state
          else
            return (programFromState state)


insertAtRef :: State -> CompileRes -> CompileRes
insertAtRef State{cpu} program = do
  let pc = getConcreteAddrMaybe (getPC cpu)
  case pc of
    Nothing -> program
    Just pc -> S_AtRef pc <$> program


getPC :: Cpu CompTime -> Exp16
getPC cpu = make_E16_HiLo HiLo{hi,lo} where
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
  sequence (reg16Updates ++ regUpdates ++ flagUpdates ++ shifterUpdates) finish
    where
      reg16Updates =
        [ S_AssignReg16 rr v
        | rr <- allReg16s
        , let v = Cpu.get16 cpu rr
        , not (v == E16_Reg rr)
        ]
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

      allReg16s = [SP,HL]
      allRegs = [A,B,C,D,E]
      allFlags = [FlagS,FlagZ,FlagA,FlagP,FlagCY]

      sequence xs = foldr (.) id xs

      finish = S_Jump a
        where
          a = make_E16_HiLo HiLo{hi = pch, lo = pcl}
          pch = Cpu.get cpu PCH
          pcl = Cpu.get cpu PCL



compileThen :: Eff CompTime () -> State -> (State -> CompileRes) -> CompileRes
compileThen semantics state k =
  run state semantics $ \state () -> k state
  where

    run :: State -> Eff CompTime a -> (State -> a -> CompileRes) -> CompileRes
    run s@State{cpu,shifter,rom} eff k = case eff of

      E.Ret x -> k s x
      E.Bind eff f -> run s eff $ \s a -> run s (f a) k

      E.GetReg16 rr -> share16 (k s) (Cpu.get16 cpu rr)
      E.SetReg16 rr a -> k s { cpu = Cpu.set16 cpu rr a } ()
      E.GetReg r -> share8 (k s) (Cpu.get cpu r)
      E.SetReg r b -> k s { cpu = Cpu.set cpu r b } ()
      E.GetFlag flag -> k s (Cpu.getFlag cpu flag)
      E.SetFlag flag v -> k s { cpu = Cpu.setFlag cpu flag v } ()

      E.ReadMem a ->
        case tryRomLookupE16 rom a of
          Just byte -> k s (E8_Lit byte)
          Nothing -> share8 (k s) (E8_ReadMem a)
      E.WriteMem a b -> S_MemWrite a b <$> k s ()

      E.GetShifterReg r -> k s (Shifter.get shifter r)
      E.SetShifterReg r b -> k s { shifter = Shifter.set shifter r b } ()

      E.EnableInterrupts -> S_EnableInterrupts <$> k s ()
      E.DisableInterrupts -> S_DisableInterrupts <$> k s ()

      E.Decode e ->
        case e of
          E8_Lit byte -> k s (decode byte)
          _ -> error $ "Decode, non-literal: " <> show e
      E.MarkReturnAddress a -> S_MarkReturnAddress a <$> k s ()
      E.TraceInstruction i -> S_TraceInstruction cpu i <$> k s ()
      E.Advance n -> S_Advance n <$> k s ()

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

      E.CaseByte e choices ->
        case e of
          E8_Lit (Byte w8) -> k s w8
          _ ->
            S_Switch8 e <$> sequence [ do
                                         prog <- k s choice
                                         pure (choice,prog)
                                     | choice <- choices
                                     ]

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
      E.UnknownOutput port byte -> S_UnknownOutput port byte <$> k s ()
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
share8 k exp =
  if atomic8 exp
  then k exp
  else do
    var <- NewAVar
    body <- k (E8_Var var)
    return $ S_Let8 var exp body

atomic8 :: Exp8 -> Bool
atomic8 = \case
  E8_Var{} -> True
  E8_Lit{} -> True
  _ -> False

share16 :: (Exp16 -> Gen Program) -> (Exp16 -> Gen Program)
share16 k exp =
  if atomic16 exp
  then k exp
  else do
    var <- NewAVar
    body <- k (E16_Var var)
    return $ S_Let16 var exp body

atomic16 :: Exp16 -> Bool
atomic16 = \case
  E16_Var{} -> True
  E16_Lit{} -> True
  _ -> False

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
