module D20Spec (spec) where

import Test.Hspec
import D20

spec :: Spec
spec = do

  describe "number of doors" $ do
    it "example 1" $ largestNumberOfDoors "^WNE$" `shouldBe` 3
    it "example 2" $ largestNumberOfDoors "^ENWWW(NEEE|SSE(EE|N))$" `shouldBe` 10
    it "example 3" $ largestNumberOfDoors "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 18
    it "example 4" $ largestNumberOfDoors "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 23
    it "example 5" $ largestNumberOfDoors "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" `shouldBe` 31
 
  describe "part 1" $ do
    it "submits answer" $ do input <- readFile "test/D20.txt"; largestNumberOfDoors input `shouldBe` 3415
    

