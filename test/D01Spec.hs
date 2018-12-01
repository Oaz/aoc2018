module D01Spec (spec) where

import Test.Hspec
import D01

spec :: Spec
spec = do

    describe "compute resulting frequency" $ do
        it "goes up" $ resultingFrequency "+1 +1 +1" `shouldBe` 3
        it "still goes up" $ resultingFrequency "+1 +1 +1 +1" `shouldBe` 4
        it "goes up with speed change" $ resultingFrequency "+1 +2 +1 +2" `shouldBe` 6
        it "goes up and down" $ resultingFrequency "+1 +1 -2" `shouldBe` 0
        it "goes down" $ resultingFrequency "-1 -2 -3" `shouldBe` -6
        it "ignores empty lines" $ resultingFrequency "-1\n\n-2\n\n-3" `shouldBe` -6
        it "submits answer" $ do
            input <- readFile "test/D01.txt"
            resultingFrequency input `shouldBe` 454

    describe "monitor past frequencies" $ do
        it "goes back to origin at the end of the initial sequence" $ frequencyReachedTwice "+1 -1" `shouldBe` 0
        it "goes back to highest point at the end of the initial sequence" $ frequencyReachedTwice "+10 -4 +4" `shouldBe` 10
        it "goes back to highest point before the end of the initial sequence" $ frequencyReachedTwice "+10 -4 +4 +5" `shouldBe` 10
        it "goes back to highest point some day" $ frequencyReachedTwice "-6 +3 +8 +5 -6" `shouldBe` 5
        it "one last example" $ frequencyReachedTwice "+7 +7 -2 -7 -4" `shouldBe` 14
        it "submits answer" $ do
            input <- readFile "test/D01.txt"
            frequencyReachedTwice input `shouldBe` 566
                  
    describe "first item seen twice" $ do
        it "simplest case" $ firstTwice [0,0] `shouldBe` 0
        it "simplest case with other number" $ firstTwice [5,5] `shouldBe` 5
        it "not in the head" $ firstTwice [0,5,5,0] `shouldBe` 5

