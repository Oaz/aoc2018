module D16Spec (spec) where

import Test.Hspec
import D16

spec :: Spec
spec = do

    describe "find executions" $ do

      it "example" $ countMatchingExecutions 
                        (Sample (Memory 3 2 1 1)
                                (Instruction 9 (Parameters 2 1 2))
                                (Memory 3 2 2 1)) `shouldBe` 3

      it "part 1" $ do
        input <- readFile "test/D16.part1.txt";
        let samples = readSamples input
        length samples `shouldBe` 812
        countSamplesWithMatchingExecutions (flip (>=) 3) samples `shouldBe` 592

    describe "part 2" $ do

      it "find opcode" $ do
        input <- readFile "test/D16.part1.txt";
        let candidates = [[9,2,7,1,8]
                        ,[9,2,14,12]
                        ,[5,0,9,6,12]
                        ,[5,9,7]
                        ,[9]
                        ,[9,7]
                        ,[5,9,6,2,7,15,1,14,12]
                        ,[5,9,2,7,15,14,12]
                        ,[9,2,7]
                        ,[3,11,9,13,4,2,1,14]
                        ,[11,9,4,2,7]
                        ,[11,9,13,4,2]
                        ,[11,9,2]
                        ,[3,11,13]
                        ,[11,7,1]
                        ,[11,10,1]]
        findOpcodeCandidates (readSamples input) `shouldBe` candidates
        reduceOpcodeCandidates candidates `shouldBe` [8,12,0,5,9,7,6,15,2,14,4,13,11,3,1,10]
      
      it "run program" $ do
        samplesInput <- readFile "test/D16.part1.txt";
        programInput <- readFile "test/D16.part2.txt";
        let device = createDevice (readSamples samplesInput)
        let program = readProgram programInput
        runProgram device program `shouldBe` Memory 557 6 0 557
  