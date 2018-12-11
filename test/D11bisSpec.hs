module D11bisSpec (spec) where

import Test.Hspec
import D11bis
import Data.Matrix

spec :: Spec
spec = do

  describe "power cells" $ do
    it "example 1"  $ getElem 3 5 (powerCells 8) `shouldBe` 4
    it "example 2"  $ getElem 122 79 (powerCells 57) `shouldBe` -5
    it "example 3"  $ getElem 217 196 (powerCells 39) `shouldBe` 0
    it "example 4"  $ getElem 101 153 (powerCells 71) `shouldBe` 4
  
  let sat1 = summedAreaTable $ powerCells 18
  let sat2 = summedAreaTable $ powerCells 42
  let sat3 = summedAreaTable $ powerCells 5791
  
  describe "highest power cell" $ do
    it "example 1"  $ highest 3 sat1 `shouldBe` ((33,45),29)
    it "example 2"  $ highest 3 sat2 `shouldBe` ((21,61),30)
    it "submits answer"  $ highest 3 sat3 `shouldBe` ((20,68),29)

  describe "ultimate highest power cell" $ do
    it "example 1"  $ ultimateHighest sat1 `shouldBe` ((90,269,16),113)
    it "example 2"  $ ultimateHighest sat2 `shouldBe` ((232,251,12),119)
    it "submits answer"  $ ultimateHighest sat3 `shouldBe` ((231,273,16),111)
  