module D14Spec (spec) where

import Test.Hspec
import D14
import Data.Sequence

spec :: Spec
spec = do

  describe "recipes" $ do
    it "1st move" $ move (Recipes 0 1 (fromList "37")) `shouldBe` (Recipes 0 1 (fromList "3710"))
    it "2nd move"  $ move (Recipes 0 1 (fromList "3710")) `shouldBe` (Recipes 4 3 (fromList "371010"))
    it "3rd move"  $ move (Recipes 4 3 (fromList "371010")) `shouldBe` (Recipes 6 4 (fromList "3710101"))
    it "4th move"  $ move (Recipes 6 4 (fromList "3710101")) `shouldBe` (Recipes 0 6 (fromList "37101012"))

    it "make 5 recipes"  $ make 5 `shouldBe` "0124515891"
    it "make 9 recipes"  $ make 9 `shouldBe` "5158916779"
    it "make 18 recipes"  $ make 18 `shouldBe` "9251071085"
    it "make 2018 recipes"  $ make 2018 `shouldBe` "5941429882"
    it "make 030121 recipes"  $ make 030121 `shouldBe` "5101271252"

    it "search 01245 in recipes"  $ search (fromList "01245") `shouldBe` 5
    it "search 51589 in recipes"  $ search (fromList "51589") `shouldBe` 9
    it "search 92510 in recipes"  $ search (fromList "92510") `shouldBe` 18
    it "search 59414 in recipes"  $ search (fromList "59414") `shouldBe` 2018
    it "search 030121 in recipes"  $ search (fromList "030121") `shouldBe` 20287556
