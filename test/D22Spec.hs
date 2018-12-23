module D22Spec (spec) where

import Test.Hspec
import D22


checkAt :: Cave -> Coordinate -> Integer -> Int -> RegionType -> SpecWith ()
checkAt c p gi el rt = describe ("example "++(show p)) $ do
                          it "geologic index" $ geologicIndex c p `shouldBe` gi
                          it "erosion level" $ erosionLevel c p `shouldBe` el
                          it "region type" $ regionType c p `shouldBe` rt

spec :: Spec
spec = do

  let sampleCave = mkCave 510 (10,10)
  checkAt sampleCave (0,0) 0 510 Rocky
  checkAt sampleCave (1,0) 16807 17317 Wet
  checkAt sampleCave (0,1) 48271 8415 Rocky
  checkAt sampleCave (1,1) 145722555 1805 Narrow

  describe "example (10,10)" $ do
    it "risk level" $ riskLevel sampleCave `shouldBe` 114

  let realCave = mkCave 3198 (12,757)
  describe "part 1" $ do
    it "risk level" $ riskLevel realCave `shouldBe` 9659
  
  describe "example (10,10)" $ do
    it "shortest path" $ shortestPathToTarget sampleCave `shouldBe` 45
  
  describe "part 2" $ do
    it "shortest path" $ shortestPathToTarget realCave `shouldBe` 1043
     