
module Play1_Symbolic(main) where

import Control.Monad (ap,liftM)

main :: IO ()
main = do
  putStrLn "*play1-symbolic*"
  let rom = Rom [ Byte 0, Byte 1, Byte 2, Byte 3, Byte 2
--                , Byte 4
                ]

  putStrLn "emulate..."
  emulate rom

  putStrLn "compile..."
  let prog = compile rom
  print prog

data Op
  = NOP
  | MES
  | OUTPUT_A
  | INC_A
  | RESET

execute :: Ops b -> Op -> Eff b (Cycles, Flow (Addr b))
execute Ops{litA,incB} = \case
  NOP -> return (Cycles 1, Next 1)
  MES -> do
    Message "hey"
    return (Cycles 1, Next 1)
  OUTPUT_A -> do
    a <- GetReg ACC
    OutputData a
    return (Cycles 1, Next 1)
  INC_A -> do
    a <- GetReg ACC
    SetReg ACC (incB a)
    return (Cycles 1, Next 1)
  RESET ->
    return (Cycles 1, Jump (litA startAddr))


decode :: Byte -> Op
decode b@(Byte n) = case n of
  0 -> NOP
  1 -> MES
  2 -> OUTPUT_A
  3 -> INC_A
  4 -> RESET
  _ -> error $ "decode: " <> show b


----------------------------------------------------------------------

data Flow a = Next Int | Jump a

getPC :: Eff b (Addr b)
getPC = do
  lo <- GetReg PCL
  hi <- GetReg PCH
  return $ Addr {lo,hi}

setPC :: Addr b -> Eff b ()
setPC Addr{lo,hi} = do
  SetReg PCL lo
  SetReg PCH hi


checkInterupt :: Cycles -> Eff v ()
checkInterupt cycles = do
  CC n1 <- GetCC
  let i1 = n1 `div` period
  Tick cycles
  CC n2 <- GetCC
  let i2 = n2 `div` period
  case i2 > i1 of
    True -> Message $ "interrupt." <> show i2
    False -> return ()
  where
    period = 7

decodeExec :: Ops b -> Addr Byte -> Byte -> Eff b (Addr b)
decodeExec ops@Ops{litA} addr byte = do
  (cycles,flow) <- execute ops (decode byte)
  checkInterupt cycles
  case flow of
    Jump addr' -> return addr'
    Next i -> return (litA (bumpAddr i addr))


-- effect-type : polymorphic over byte-type `b` as well as `a`
data Eff b a where
  Ret :: a -> Eff b a
  Bind :: Eff b a -> (a -> Eff b a2) -> Eff b a2
  GetReg :: Reg -> Eff b b
  SetReg :: Reg -> b -> Eff b ()
  ReadMem :: Addr b -> Eff b b
  Message :: String -> Eff b ()
  OutputData :: b -> Eff b ()
  GetCC :: Eff b CycleCount
  Tick :: Cycles -> Eff b ()

instance Functor (Eff b) where fmap = liftM
instance Applicative (Eff b) where pure = return; (<*>) = ap
instance Monad (Eff b) where return = Ret; (>>=) = Bind

-- operations-type : polymorphic over byte-type `b`
data Ops b = Ops
  { litA :: Addr Byte -> Addr b
  , lit :: Byte -> b
  , incB :: b -> b
  }


----------------------------------------------------------------------
-- emulate

emulate :: Rom -> IO ()
emulate rom = run (state0 rom) theSemantics $ \_ a -> return a
  where

    theSemantics :: Eff Byte ()
    theSemantics = do
      setPC startAddr
      loop
        where
          loop = do
            a0 <- getPC
            byte <- ReadMem a0
            a1 <- decodeExec opsByte a0 byte
            setPC a1
            loop

    run :: Machine Byte -> Eff Byte a -> (Machine Byte -> a -> IO ()) -> IO ()
    run s@Machine{cpu,mem,cc} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k
      GetReg r -> k s (getReg cpu r)
      SetReg r b -> k s { cpu = setReg cpu r b} ()
      ReadMem a -> k s (readMem opsByte mem a)
      Message str -> do
        put s $ "Message: " <> str
        k s ()
      OutputData b -> do
        put s $ "OutputData: " <> show b
        k s ()
      GetCC -> k s cc
      Tick cycles -> k s { cc = tick cc cycles } ()


    put :: Machine Byte -> String -> IO ()
    put Machine{cc,cpu} str =
      putStrLn $ "[" <> show (cc,cpu)  <> "] : " <> str

state0 :: Rom -> Machine Byte
state0 rom = Machine { cpu = cpu0, mem = mem0 rom, cc = cc0 }

mem0 :: Rom -> Mem Byte
mem0 rom = Mem { rom, ram = ram0 byte0 }

cpu0 :: Cpu Byte
cpu0 = Cpu { acc = byte0, pcl = byte0, pch = byte0 }



----------------------------------------------------------------------
-- compile

compile :: Rom -> Prog
compile rom = Prog [(Lab a, semAt a) | a <- romAddrs rom ]
  where

    semAt :: Addr Byte -> Stat
    semAt a0 = do
      let byte = fetch rom a0
      run (decodeExec opsExp a0 byte) $ \a1 ->
        case unflexAddr a1 of
          Nothing -> S_ComputedGoto a1
          Just a1 -> S_Goto (Lab a1)

    run :: Eff Exp a -> (a -> Stat) -> Stat
    run eff k = case eff of
      Ret a -> k a
      Bind eff f -> run eff $ \a -> run (f a) k
      GetReg r -> k (E_GetReg r)
      SetReg r e -> S_AssignThen r e $ k ()
      ReadMem a -> k (E_ReadMem a)
      Message str -> S_Message str $ k ()
      OutputData b -> S_OutputData b $ k ()
      GetCC{} -> undefined
      Tick{} -> undefined



opsExp :: Ops Exp
opsExp = Ops
  { litA = flexAddr
  , lit = E_Lit
  , incB = E_Inc
  }

flexAddr :: Addr Byte -> Addr Exp
flexAddr Addr{lo,hi} = Addr {lo = E_Lit lo, hi = E_Lit hi}

unflexAddr :: Addr Exp -> Maybe (Addr Byte)
unflexAddr = \case
  Addr{lo = E_Lit lo, hi = E_Lit hi} -> Just (Addr {lo,hi})
  Addr{} -> Nothing



data Prog = Prog [(Lab,Stat)]

data Stat
  = S_Goto Lab
  | S_ComputedGoto (Addr Exp)
  | S_AssignThen Reg Exp Stat
  | S_Message String Stat
  | S_OutputData Exp Stat

data Lab = Lab (Addr Byte)

data Exp
  = E_Lit Byte
  | E_Inc Exp
  | E_GetReg Reg
  | E_ReadMem (Addr Exp)

instance Show Prog where show = unlines . prettyProg
instance Show Stat where show = unlines . prettyStat
instance Show Exp where show = prettyExp
instance Show Lab where show (Lab a) = "L_" ++ show a

prettyProg :: Prog -> [String]
prettyProg (Prog xs) =
  concat [ [show lab ++ ":"] ++ indent (prettyStat stat) | (lab,stat) <- xs ]

prettyStat :: Stat -> [String]
prettyStat = \case
  S_Message str stat -> ("message(" ++ show str ++ ")") : prettyStat stat
  S_OutputData exp stat -> ("outputData(" ++ show exp ++ ")") : prettyStat stat
  S_ComputedGoto a -> ["goto-computed: " ++ show a]
  S_Goto lab -> ["goto: " ++ show lab]
  S_AssignThen r e s ->
    [  show r ++ ":=" ++ show e ] ++
    prettyStat s

indent :: [String] -> [String]
indent lines = ["  " ++ line | line <- lines ]

prettyExp :: Exp -> String
prettyExp = \case
  E_Lit byte -> show byte
  E_GetReg r -> show r
  E_Inc e -> brac (show e ++ "+1")
  E_ReadMem a -> "mem["++ show a ++ "]"
  where brac s = "("++s++")"


----------------------------------------------------------------------
-- byte-polymorphic types and ops


data Machine b = Machine { cpu :: Cpu b, mem :: Mem b, cc :: CycleCount }
data Cpu b = Cpu { acc :: b, pcl :: b, pch :: b } deriving Show
data Mem b = Mem { rom :: Rom, ram :: Ram b }
data Ram b = Ram


ram0 :: b -> Ram b
ram0 = undefined Ram

data Reg = PCL | PCH | ACC
  deriving Show

getReg :: Cpu b -> Reg -> b
getReg Cpu{acc,pcl,pch} = \case
  ACC -> acc
  PCL -> pcl
  PCH -> pch

setReg :: Cpu b -> Reg -> b -> Cpu b
setReg cpu = \case
  ACC -> \acc -> cpu { acc }
  PCL -> \pcl -> cpu { pcl }
  PCH -> \pch -> cpu { pch }

readMem :: Ops b -> Mem b -> Addr Byte -> b
readMem Ops{lit} Mem{rom} = \case
  a -> lit (fetch rom a)

--writeMem :: Mem b -> Addr Byte -> b -> Mem b
--writeMem = undefined


data Addr b = Addr { lo :: b, hi :: b }

instance Show b => Show (Addr b) where show Addr {lo,hi} = show hi <> "." <> show lo


----------------------------------------------------------------------

newtype Cycles = Cycles Int

newtype CycleCount = CC Int
  deriving Show

cc0 :: CycleCount
cc0 = CC 0

tick :: CycleCount -> Cycles -> CycleCount
tick (CC n1) (Cycles n2) = CC (n1+n2)

----------------------------------------------------------------------
-- Byte-fixed types and op

data Rom = Rom [Byte]


fetch :: Rom -> Addr Byte -> Byte
fetch rom = \case
  Addr{lo,hi=Byte 0} -> readRom rom lo
  a@Addr{} -> error $ "fetch: " <> show a


readRom :: Rom -> Byte -> Byte
readRom (Rom bytes) (Byte n) =
  if n >= length bytes then error $ "readRom: n=" <> show n else
  bytes !! n

romAddrs :: Rom -> [Addr Byte]
romAddrs (Rom bytes) = [ Addr{lo=Byte n,hi=byte0} | n <- [0 .. (length bytes - 1) ] ]

startAddr :: Addr Byte
startAddr = Addr { lo = byte0, hi = byte0 }

bumpAddr :: Int -> Addr Byte -> Addr Byte
bumpAddr i Addr{lo,hi} = Addr { lo = incByte i lo, hi }



data Byte = Byte Int
instance Show Byte where show (Byte n) = show n


byte0 :: Byte
byte0 = Byte 0

incByte :: Int -> Byte -> Byte
incByte i (Byte n) = Byte (i+n)

opsByte :: Ops Byte
opsByte = Ops
  { litA = id
  , lit = id
  , incB = incByte 1
  }
