module D03Spec (spec) where

import Test.Hspec
import D03
import Data.Complex

spec :: Spec
spec = do

  describe "utilities" $ do
    it "parses into claims" $ mkClaims "#1 @ 146,196: 19x14\n#2 @ 641,817: 27x28" `shouldBe` [ Claim 1 (146:+196) (164:+209), Claim 2 (641:+817) (667:+844) ]

  describe "intersections size" $ do
    it "simple example" $ claimsIntersectionSize "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" `shouldBe` 4
    it "other example" $ claimsIntersectionSize "#1 @ 1,2: 3x3\n#2 @ 2,3: 3x4\n#3 @ 1,5: 3x3" `shouldBe` 8
    it "overlap example" $ claimsIntersectionSize "#1 @ 1,2: 3x3\n#2 @ 2,3: 3x4\n#3 @ 1,5: 3x3\n#4 @ 2,5: 2x3" `shouldBe` 10
    it "other overlap example" $ claimsIntersectionSize "#1 @ 1,2: 3x3\n#2 @ 2,3: 3x4\n#3 @ 1,5: 3x3\n#4 @ 2,5: 2x3\n#5 @ 2,2: 3x3" `shouldBe` 14
    it "submits answer" $ do input <- readFile "test/D03.txt"; claimsIntersectionSize input `shouldBe` 106501

  describe "isolated claim" $ do
    it "simple example" $ isolatedClaim "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" `shouldBe` 3
    it "submits answer" $ do input <- readFile "test/D03.txt"; isolatedClaim input `shouldBe` 632
  


  
