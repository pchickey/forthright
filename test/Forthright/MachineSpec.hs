module Forthright.MachineSpec where

import Test.Hspec
import qualified Data.Map as Map

import Forthright.Instruction
import Forthright.Machine

spec :: Spec
spec = do
  trivialHaltSpec

trivialHaltSpec :: Spec
trivialHaltSpec = describe "halting behavior" $ do
  it "a trivial program halts in one step" $
    let p = mkprog [(0, InstrFlow FlowHalt)]
        m = machineStep p minit
    in machineHalted <$> m `shouldBe` Right True
  where
  minit = Machine
    { machinePC = IRef 0
    , machineK = []
    , machineD = []
    , machineHalted = False
    }

  mkprog p = Map.fromList (map (\(l,i) -> (IRef l, i)) p)
