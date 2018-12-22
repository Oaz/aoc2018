module D22Spec (spec) where

import Test.Hspec
import D22


checkAt :: Cave -> Coordinate -> Integer -> Int -> Int -> SpecWith ()
checkAt c p gi el rt = describe ("example "++(show p)) $ do
                          it "geologic index" $ geologicIndex c p `shouldBe` gi
                          it "erosion level" $ erosionLevel c p `shouldBe` el
                          it "region type" $ regionType c p `shouldBe` rt

spec :: Spec
spec = do

  let sampleCave = mkCave 510 (10,10)
  checkAt sampleCave (0,0) 0 510 0
  checkAt sampleCave (1,0) 16807 17317 1
  checkAt sampleCave (0,1) 48271 8415 0
  checkAt sampleCave (1,1) 145722555 1805 2

  describe "example (10,10)" $ do
    it "risk level" $ riskLevel sampleCave `shouldBe` 114

  let realCave = mkCave 3198 (12,757)
  describe "part 1" $ do
    it "risk level" $ riskLevel realCave `shouldBe` 9659