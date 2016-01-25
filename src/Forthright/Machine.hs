module Forthright.Machine
  ( IRef(..)
  , Machine(..)
  , Err(..)
  , machineStep
  ) where

import qualified Data.Map as Map
import           Data.Map   (Map)
import           Data.Word  (Word32)
import           Data.Int   (Int32)

import           Forthright.Instruction

type Prog = Map IRef (Instr IRef)

newtype IRef = IRef
  { unIRef :: Word32
  } deriving (Eq, Show, Ord)

incrIRef :: IRef -> IRef
incrIRef (IRef i) = IRef (i+1)

data Machine = Machine
  { machinePC     :: IRef
  , machineK      :: [IRef]
  , machineD      :: [Word32]
  , machineHalted :: Bool
  } deriving (Eq, Show)

data Err
  = ErrInvalidInstruction
  | ErrEmptyK
  | ErrEmptyD
  deriving (Eq, Show)

machineStep :: Prog -> Machine -> Either Err Machine
machineStep p m =
  -- precondition: PC points to a valid instruction
  case Map.lookup (machinePC m) p of
    Nothing            -> Left ErrInvalidInstruction
    Just (InstrFlow f) -> flowStep f m
    Just (InstrOp o)   -> case opStep o (machineD m) of
      Right d' -> Right (nextPC m { machineD = d' })
      Left  e  -> Left e
  where
  flowStep :: Flow IRef -> Machine -> Either Err Machine
  -- precondition for next three ops: true
  flowStep (FlowPushK k) m = Right $ nextPC m { machineK = k : machineK m }
  flowStep (FlowJump pc) m = Right $ m { machinePC = pc }
  flowStep FlowHalt      m = Right $ m { machineHalted = True }
  -- precondition: at least one item on K stack
  flowStep FlowContinue  m = continue m
  -- precondition: at least one item on D stack
  flowStep (FlowCond r)  m = cond r m


  continue m | (k:ks) <- machineK m = Right m { machinePC = k, machineK = ks }
             | otherwise            = Left ErrEmptyK

  cond j m | (0:_) <- machineD m = Right m { machinePC = j }
           | (_:_) <- machineD m = Right (nextPC m)
           | otherwise           = Left ErrEmptyD

  opStep :: Op -> [Word32] -> Either Err [Word32]
  -- Precondition: true
  opStep (OpPush d) ds      = Right (d:ds)
  -- Precondition for next two ops: at least one item on D stack
  opStep OpPop     (_:ds)   = Right ds
  opStep OpDup     (d:ds)   = Right (d:d:ds)
  -- Precondition for remaining ops:: at least two items on D stack
  opStep OpSwap    (a:b:ds) = Right (b:a:ds)
  opStep (OpAdd s) (a:b:ds) = Right ((add s a b):ds)
  opStep (OpSub s) (a:b:ds) = Right ((sub s a b):ds)
  opStep (OpMul s) (a:b:ds) = Right ((mul s a b):ds)
  opStep _         _        = Left ErrEmptyD

  nextPC :: Machine -> Machine
  nextPC m = m { machinePC = incrIRef (machinePC m) }

  -- overS taking two functions is super ugly, but i don't feel like convincing
  -- the haskell typechecker about what i mean this afternoon
  add = overS (+) (+)
  sub = overS (-) (-)
  mul = overS (*) (*)
  overS f _ Unsigned a b = f a b
  overS _ f Signed   a b = fromIntegral (f (fromIntegral a) (fromIntegral b))
