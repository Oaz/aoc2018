module D21Spec (spec) where

import Test.Hspec
import D21
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
    
    describe "part 1" $ do

      it "run hacked program" $ do
        programInput <- readFile "test/D21.txt";
        let program = hackProgram1 $ readProgram programInput
        endProgram initialMemory program `shouldBe` Memory 30 0 30 1 1 15823996 0 M.empty
        
      it "run program with hacked memory" $ do
        programInput <- readFile "test/D21.txt";
        let program = readProgram programInput
        let hackedMemory = setR 0 15823996 initialMemory
        endProgram hackedMemory program `shouldBe` Memory 31 15823996 31 1 1 15823996 1 M.empty
  
    
    describe "part 2" $ do

      it "run hacked program" $ do
        programInput <- readFile "test/D21.txt";
        let program = hackProgram2 $ readProgram programInput
        trace (endProgram initialMemory program) `shouldBe` M.singleton 10199686 11456
        
      it "run program with hacked memory" $ do
        programInput <- readFile "test/D21.txt";
        let program = readProgram programInput
        let hackedMemory = setR 0 10199686 initialMemory
        endProgram hackedMemory program `shouldBe` Memory 31 10199686 31 1 195 10199686 1 M.empty
       