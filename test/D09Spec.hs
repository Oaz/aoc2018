module D09Spec (spec) where

import Test.Hspec
import D09
import qualified Data.Sequence as S

sq x = S.fromList x

spec :: Spec
spec = do

    describe "circles" $ do
      it "round 0"  $ (marbleRounds (23,7)) !! 0 `shouldBe` MarbleCircle 0 0 (sq [0]) 0
      it "round 1"  $ (marbleRounds (23,7)) !! 1 `shouldBe` MarbleCircle 1 1 (sq [0,1]) 0
      it "round 2"  $ (marbleRounds (23,7)) !! 2 `shouldBe` MarbleCircle 2 1 (sq [0,2,1]) 0
      it "round 3"  $ (marbleRounds (23,7)) !! 3 `shouldBe` MarbleCircle 3 3 (sq [0,2,1,3]) 0
      it "round 4"  $ (marbleRounds (23,7)) !! 4 `shouldBe` MarbleCircle 4 1 (sq [0,4,2,1,3]) 0
      it "round 5"  $ (marbleRounds (23,7)) !! 5 `shouldBe` MarbleCircle 5 3 (sq [0,4,2,5,1,3]) 0
      it "round 6"  $ (marbleRounds (23,7)) !! 6 `shouldBe` MarbleCircle 6 5 (sq [0,4,2,5,1,6,3]) 0
      it "scoring round"  $ (marbleRounds (6,2)) !! 6 `shouldBe`             MarbleCircle 6 1 (sq [0,2,5,1,3]) (6+4)
      it "before 2nd scoring round"  $ (marbleRounds (6,2)) !! 11 `shouldBe` MarbleCircle 11 3 (sq [0,10,2,11,5,7,1,8,3,9]) 0
      it "2nd scoring round"  $ (marbleRounds (6,2)) !! 12 `shouldBe`        MarbleCircle 12 1 (sq [0,2,11,5,7,1,8,3,9]) (12+10)
      it "before 3rd scoring round"  $ (marbleRounds (6,2)) !! 17 `shouldBe` MarbleCircle 17 11 (sq [0,2,11,13,5,14,7,15,1,16,8,17,3,9]) 0
      it "3rd scoring round"  $ (marbleRounds (6,2)) !! 18 `shouldBe`        MarbleCircle 18 9 (sq [0,2,11,13,5,14,7,15,1,8,17,3,9]) (18+16)
      it "scoring round with big shift"  $ (marbleRounds (6,5)) !! 6 `shouldBe`  MarbleCircle 6 4 (sq [0,4,2,5,3]) (6+1)
      it "before scoring round at 1st index"  $ (marbleRounds (4,3)) !! 3 `shouldBe`  MarbleCircle 3 3 (sq [0,2,1,3]) 0
      it "scoring round at 1st index"  $ (marbleRounds (4,3)) !! 4 `shouldBe`  MarbleCircle 4 4 (sq [2,1,3]) (4+0)

    describe "game" $ do
      it "example 1" $ highestScore 9 23 `shouldBe` 32
      it "example 2" $ highestScore 10 1618 `shouldBe` 8317
      it "example 3" $ highestScore 13 7999 `shouldBe` 146373
      it "example 4" $ highestScore 17 1104 `shouldBe` 2764
      it "example 5" $ highestScore 21 6111 `shouldBe` 54718
      it "example 6" $ highestScore 30 5807 `shouldBe` 37305
      it "submits answer" $ highestScore 403 71920 `shouldBe` 439089
      it "submits answer again" $ highestScore 403 7192000 `shouldBe` 3668541094
