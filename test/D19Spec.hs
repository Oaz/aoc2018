module D19Spec (spec) where

import Test.Hspec
import D19
import qualified Data.Vector as V

spec :: Spec
spec = do
    
    describe "part 1" $ do

      it "run test program" $ do
        let program n = Program (registerIP 0) $ V.fromListN n  [ Instruction runSeti (Parameters 5 0 1)
                                                                , Instruction runSeti (Parameters 6 0 2)
                                                                , Instruction runAddi (Parameters 0 1 0)
                                                                , Instruction runAddr (Parameters 1 2 3)
                                                                , Instruction runSetr (Parameters 1 0 0)
                                                                , Instruction runSeti (Parameters 8 0 4)
                                                                , Instruction runSeti (Parameters 9 0 5)
                                                                ]
        endProgram initialMemory (program 1) `shouldBe` Memory 1 1 5 0 0 0 0
        endProgram initialMemory (program 2) `shouldBe` Memory 2 2 5 6 0 0 0
        endProgram initialMemory (program 3) `shouldBe` Memory 4 4 5 6 0 0 0
        endProgram initialMemory (program 5) `shouldBe` Memory 6 6 5 6 0 0 0
        endProgram initialMemory (program 7) `shouldBe` Memory 7 7 5 6 0 0 9

      it "run program" $ do
        programInput <- readFile "test/D19.txt";
        let program = readProgram programInput
        let program' = hackProgram program
        endProgram initialMemory program' `shouldBe` Memory 257 1872 1 1030 2 1031 257
        
    describe "part 2" $ do

      it "run program" $ do
        programInput <- readFile "test/D19.txt";
        let program = readProgram programInput
        let program' = hackProgram program
        endProgram alteredMemory program' `shouldBe` Memory 257 18992592 1 10551430 2 10551431 257
 