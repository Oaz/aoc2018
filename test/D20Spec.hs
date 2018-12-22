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
    
  describe "count rooms" $ do
    it "example 1a" $ countRoomsAfterDoors 1 "^WNE$" `shouldBe` 3
    it "example 1b" $ countRoomsAfterDoors 2 "^WNE$" `shouldBe` 2
    it "example 1c" $ countRoomsAfterDoors 3 "^WNE$" `shouldBe` 1
    it "example 2a" $ countRoomsAfterDoors 3 "^ENWWW(NEEE|SSE)$" `shouldBe` 10
    it "example 2b" $ countRoomsAfterDoors 6 "^ENWWW(NEEE|SSE)$" `shouldBe` 7
    it "example 2c" $ countRoomsAfterDoors 8 "^ENWWW(NEEE|SSE)$" `shouldBe` 3
    it "example 2d" $ countRoomsAfterDoors 8 "^ENWWW(NEEE|SSE(EE|N))$" `shouldBe` 6
    it "example 2e" $ countRoomsAfterDoors 10 "^ENWWW(NEEE|SSE(EE|N))$" `shouldBe` 1
    it "example 3a" $ countRoomsAfterDoors 6 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 19
    it "example 3b" $ countRoomsAfterDoors 8 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 17
    it "example 3c" $ countRoomsAfterDoors 11 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 12
    it "example 3d" $ countRoomsAfterDoors 15 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" `shouldBe` 7
    it "example 4a" $ countRoomsAfterDoors 12 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 23
    it "example 4b" $ countRoomsAfterDoors 17 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 12
    it "example 4c" $ countRoomsAfterDoors 19 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 7
    it "example 4d" $ countRoomsAfterDoors 22 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" `shouldBe` 2
    it "example 5a" $ countRoomsAfterDoors 22 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" `shouldBe` 26
 
  describe "part 2" $ do
    it "submits answer" $ do input <- readFile "test/D20.txt"; countRoomsAfterDoors 1000 input `shouldBe` 8583
