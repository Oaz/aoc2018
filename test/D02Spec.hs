module D02Spec (spec) where

import Test.Hspec
import D02

spec :: Spec
spec = do

    describe "boxes checksum" $ do
      it "no 2's or 3's" $ boxesChecksum "ab cd" `shouldBe` 0
      it "2 but no 3's" $ boxesChecksum "aba cd" `shouldBe` 0
      it "3 but no 2's" $ boxesChecksum "abaca cd" `shouldBe` 0
      it "one 2 and one 3" $ boxesChecksum "abaca cdc" `shouldBe` 1
      it "two 2 and one 3" $ boxesChecksum "abaca cdc ghg" `shouldBe` 2
      it "two 2 and two 3" $ boxesChecksum "abaca cdc ghghg" `shouldBe` 4
      it "bigger example" $ boxesChecksum "abcdef bababc abbcde abcccd aabcdd abcdee ababab" `shouldBe` 12
      it "submits answer" $ do input <- readFile "test/D02.txt"; boxesChecksum input `shouldBe` 8610


    describe "common letters" $ do
      it "example" $ commonLetters "abcde fghij klmno pqrst fguij axcye wvxyz" `shouldBe` "fgij"
      it "submits answer yolo" $ do input <- readFile "test/D02.txt"; commonLetters input `shouldBe` "iosnxmfkpabcjpdywvrtahluy"
  
