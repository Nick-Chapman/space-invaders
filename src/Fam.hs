
module Fam where

import Control.Monad (ap,liftM)

executeIncOp :: Eff p Int
executeIncOp = do
  a <- GetReg R1
  a' <- IncByte a
  SetReg R1 a'
  return 2

maybeInterrupt :: Eff p ()
maybeInterrupt = do
  c <- GetCycles
  b <- CyclesIsMult 7 c
  if b then Message "boo!" else return ()

theEffect1 :: Eff p ()
theEffect1 = do
  n <- executeIncOp
  Tick n
  maybeInterrupt

theEffect :: Eff p ()
theEffect =
  sequence_ (replicate 10 theEffect1)


class Phase p where
  type Byte p
  --type Addr p
  type Cycles p


data Eff p a where
  Ret :: a -> Eff p a
  Bind :: Eff p a -> (a -> Eff p a2) -> Eff p a2
  GetReg :: Reg -> Eff p (Byte p)
  SetReg :: Reg -> Byte p -> Eff p ()
  --OutputData :: Byte p -> Eff p ()
  --ReadMem :: Addr p -> Eff p b
  IncByte :: Byte p -> Eff p (Byte p)

  Tick :: Int -> Eff p ()
  GetCycles :: Eff p (Cycles p)
  CyclesIsMult :: Int -> Cycles p -> Eff p Bool
  Message :: String -> Eff p ()

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

data Reg = R1 | R2

----------------------------------------------------------------------
-- emulation


data EmuTime

instance Phase EmuTime where
  type Byte EmuTime = ByteE
  --type Addr EmuTime = Int
  type Cycles EmuTime = Int

emulate :: Eff EmuTime () -> IO ()
emulate eff = run stateE0 eff $ \_ a -> return a
  where

    run :: StateE -> Eff EmuTime a -> (StateE -> a -> IO ()) -> IO ()
    run s@StateE{cpu,cc} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k
      GetReg r -> k s (getReg cpu r)
      SetReg r b -> k s { cpu = setReg cpu r b} ()
      Message str -> do
        put s $ "Message: " <> str
        k s ()
      IncByte b -> k s (incByteE b)
      GetCycles -> k s cc
      Tick n -> k s { cc = cc+n } ()
      CyclesIsMult m c -> k s (c `mod` m == 0)

    put :: StateE -> String -> IO ()
    put state str =
      putStrLn $ "[" <> show state  <> "] : " <> str


data StateE = StateE { cpu :: Cpu ByteE, cc :: Int }
  deriving Show

stateE0 :: StateE
stateE0 = StateE { cpu = cpu0 byteE0, cc = 0 }

data ByteE = ByteE Int

incByteE :: ByteE -> ByteE
incByteE (ByteE n) = ByteE (n+1)

byteE0 :: ByteE
byteE0 = ByteE 0

----------------------------------------------------------------------
--compilation

data CompTime

instance Phase CompTime where
  type Byte CompTime = Exp
  --type Addr CompTime = ()
  type Cycles CompTime = () -- there is just a single current cycles var

compile :: Eff CompTime () -> Stat
compile eff = run stateC0 eff $ \_ () -> S_Halt
  where

    run :: StateC -> Eff CompTime a -> (StateC -> a -> Stat) -> Stat
    run s@StateC{cpu,cc} eff k = case eff of
      Ret x -> k s x
      Bind eff f -> run s eff $ \s a -> run s (f a) k
      GetReg r -> k s (getReg cpu r)
      SetReg r b -> k s { cpu = setReg cpu r b} ()
      Message str -> S_Message str $ k s ()
      IncByte b -> k s (E_Inc b)
      GetCycles -> k s cc
      Tick n -> S_Tick n (k s ())
      CyclesIsMult n () ->
        S_IfCyclesIsMult n (k s True) (k s False)


data StateC = StateC { cpu :: Cpu Exp, cc :: () }
  deriving Show

stateC0 :: StateC
stateC0 = StateC { cpu = cpu0 E_Undefined, cc = () }


data Stat
  = S_Halt
  | S_AssignThen Reg Exp Stat
  | S_Message String Stat
  | S_Tick Int Stat
  | S_IfCyclesIsMult Int Stat Stat

data Exp
  = E_Inc Exp
  | E_Undefined

----------------------------------------------------------------------

-- content-polymorphic Cpu

data Cpu a = Cpu
  deriving Show

cpu0 :: a -> Cpu a
cpu0 = undefined

getReg :: Cpu a -> Reg -> a
getReg = undefined

setReg :: Cpu a -> Reg -> a -> Cpu a
setReg = undefined
