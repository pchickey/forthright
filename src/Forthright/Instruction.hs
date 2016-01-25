module Forthright.Instruction where

import           Data.Word  (Word32)

data Instr iref
  = InstrFlow     (Flow iref) -- Only changes the K stack, may depend on D
  | InstrOp       Op          -- Only changes the D stack, independent of K
  deriving (Eq, Show)

data Flow iref
  = FlowPushK    iref -- push value onto k stack
  | FlowJump     iref -- jump to literal
  | FlowCond     iref -- Conditionally jump when D stack is 0
                      -- (no change to K stack)
  | FlowContinue      -- pop value off k stack, jump to it
  | FlowHalt          -- halt the machine
  deriving (Eq, Show)

data Sign
  = Signed   -- treat data as 2's complement signed
  | Unsigned -- treat data as unsigned
  deriving (Eq, Show)

data Op
  = OpPush Word32 -- push value onto d stack
  | OpPop         -- pop value off d stack, throw away
  | OpDup         -- pop value off d stack, push it twice
  | OpSwap        -- pop two values off d, push in opposite order
  | OpAdd Sign    -- pop two values off d, push their sum
  | OpSub Sign    -- pop two values off d, push their difference
  | OpMul Sign    -- pop two values off d, push their product
  deriving (Eq, Show)

