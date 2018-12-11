module D09bisSpec (spec) where

import Test.Hspec
import D09bis

rb x = bfromList x

spec :: Spec
spec = do

    describe "rotating bezel" $ do
      it "eq"  $ (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [5] 1 [2,3,4])
      it "cl1"  $ rotate 5 (RotatingBezel [] 1 []) `shouldBe` (RotatingBezel [] 1 [])
      it "cl2"  $ rotate 1 (RotatingBezel [5,4,3,2] 1 []) `shouldBe` (RotatingBezel [1] 2 [3,4,5])
      it "cl3"  $ rotate 1 (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [1,5,4] 2 [3])
      it "cl4"  $ rotate 4 (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [4,3] 5 [1,2])
      it "ccl1"  $ rotate (-5) (RotatingBezel [] 1 []) `shouldBe` (RotatingBezel [] 1 [])
      it "ccl2"  $ rotate (-1) (RotatingBezel [] 1 [2,3,4,5]) `shouldBe` (RotatingBezel [4,3,2] 5 [1])
      it "ccl3"  $ rotate (-1) (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [4] 5 [1,2,3])
      it "ccl4"  $ rotate (-4) (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [1,5] 2 [3,4])
      it "insert"  $ binsert 9 (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [1,5,4] 9 [2,3])
      it "delete"  $ bdelete (RotatingBezel [5,4] 1 [2,3]) `shouldBe` (RotatingBezel [5,4] 2 [3])
      it "delete"  $ bdelete (RotatingBezel [5,4,3,2] 1 []) `shouldBe` (RotatingBezel [] 2 [3,4,5])

    describe "circles" $ do
      it "round 0"  $ (marbleRounds (23,7)) !! 0 `shouldBe` MarbleCircle 0 (rb [0]) 0
      it "round 1"  $ (marbleRounds (23,7)) !! 1 `shouldBe` MarbleCircle 1 (rb [1,0]) 0
      it "round 2"  $ (marbleRounds (23,7)) !! 2 `shouldBe` MarbleCircle 2 (rb [2,1,0]) 0
      it "round 3"  $ (marbleRounds (23,7)) !! 3 `shouldBe` MarbleCircle 3 (rb [3,0,2,1]) 0
      it "round 4"  $ (marbleRounds (23,7)) !! 4 `shouldBe` MarbleCircle 4 (rb [4,2,1,3,0]) 0
      it "round 5"  $ (marbleRounds (23,7)) !! 5 `shouldBe` MarbleCircle 5 (rb [5,1,3,0,4,2]) 0
      it "round 6"  $ (marbleRounds (23,7)) !! 6 `shouldBe` MarbleCircle 6 (rb [6,3,0,4,2,5,1]) 0
      it "scoring round"  $ (marbleRounds (6,2)) !! 6 `shouldBe`             MarbleCircle 6 (rb [2,5,1,3,0]) (6+4)
      it "before 2nd scoring round"  $ (marbleRounds (6,2)) !! 11 `shouldBe` MarbleCircle 11 (rb [11,5,7,1,8,3,9,0,10,2]) 0
      it "2nd scoring round"  $ (marbleRounds (6,2)) !! 12 `shouldBe`        MarbleCircle 12 (rb [2,11,5,7,1,8,3,9,0]) (12+10)
      it "before 3rd scoring round"  $ (marbleRounds (6,2)) !! 17 `shouldBe` MarbleCircle 17 (rb [17,3,9,0,2,11,13,5,14,7,15,1,16,8]) 0
      it "3rd scoring round"  $ (marbleRounds (6,2)) !! 18 `shouldBe`        MarbleCircle 18 (rb [8,17,3,9,0,2,11,13,5,14,7,15,1]) (18+16)
      it "scoring round with big shift"  $ (marbleRounds (6,5)) !! 6 `shouldBe`  MarbleCircle 6 (rb [3,0,4,2,5]) (6+1)
      it "before scoring round at 1st index"  $ (marbleRounds (4,3)) !! 3 `shouldBe`  MarbleCircle 3 (rb [3,0,2,1]) 0
      it "scoring round at 1st index"  $ (marbleRounds (4,3)) !! 4 `shouldBe`  MarbleCircle 4 (rb [2,1,3]) (4+0)

    describe "game" $ do
      it "example 1" $ highestScore 9 23 `shouldBe` 32
      it "example 2" $ highestScore 10 1618 `shouldBe` 8317
      it "example 3" $ highestScore 13 7999 `shouldBe` 146373
      it "example 4" $ highestScore 17 1104 `shouldBe` 2764
      it "example 5" $ highestScore 21 6111 `shouldBe` 54718
      it "example 6" $ highestScore 30 5807 `shouldBe` 37305
      it "submits answer" $ highestScore 403 71920 `shouldBe` 439089
      it "submits answer again" $ highestScore 403 7192000 `shouldBe` 3668541094
