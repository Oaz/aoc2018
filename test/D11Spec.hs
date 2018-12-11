module D11Spec (spec) where

import Test.Hspec
import D11
import Data.Matrix

spec :: Spec
spec = do

  describe "power cells" $ do
    it "example 1"  $ getElem 3 5 (powerCells 8) `shouldBe` 4
    it "example 2"  $ getElem 122 79 (powerCells 57) `shouldBe` -5
    it "example 3"  $ getElem 217 196 (powerCells 39) `shouldBe` 0
    it "example 4"  $ getElem 101 153 (powerCells 71) `shouldBe` 4
  
  describe "highest power cell" $ do
    it "example 1"  $ highestPowerCell 3 18 `shouldBe` ((33,45),29)
    it "example 2"  $ highestPowerCell 3 42 `shouldBe` ((21,61),30)
    it "submits answer"  $ highestPowerCell 3 5791 `shouldBe` ((20,68),29)

  describe "ultimate highest power cell" $ do
    it "example 1"  $ ultimateHighestPowerCell 18 `shouldBe` ((90,269,16),113)
    it "example 2"  $ ultimateHighestPowerCell 42 `shouldBe` ((232,251,12),119)
    it "submits answer"  $ ultimateHighestPowerCell 5791 `shouldBe` ((231,273,16),111)
  