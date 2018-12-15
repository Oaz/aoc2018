module D14bisSpec (spec) where

import Test.Hspec
import D14bis
import Data.Sequence

spec :: Spec
spec = do

  describe "recipes" $ do
    it "1st move" $ move (Recipes Empty '3' Empty '7' Empty)
                      `shouldBe` (Recipes Empty '3' Empty '7' (fromList "10"))
    it "2nd move" $ move (Recipes Empty '3' Empty '7' (fromList "10"))
                      `shouldBe` (Recipes (fromList "371") '0' Empty '1' (fromList "0"))
    it "3rd move" $ move (Recipes (fromList "371") '0' Empty '1' (fromList "0"))
                      `shouldBe` (Recipes (fromList "3710") '1' (fromList "0") '1' Empty)
    it "4th move" $ move (Recipes (fromList "3710") '1' (fromList "0") '1' Empty)
                      `shouldBe` (Recipes Empty '3' (fromList "71010") '1' (fromList "2"))
    it "5th move" $ move (Recipes Empty '3' (fromList "71010") '1' (fromList "2"))
                      `shouldBe` (Recipes (fromList "3710") '1' (fromList "012") '4' Empty)
    it "6th move" $ move (Recipes (fromList "3710") '1' (fromList "012") '4' Empty)
                      `shouldBe` (Recipes (fromList "371") '0' (fromList "10") '1' (fromList "245"))
    it "7th move" $ move (Recipes (fromList "371") '0' (fromList "10") '1' (fromList "245"))
                      `shouldBe` (Recipes (fromList "3710") '1' (fromList "012") '4' (fromList "51"))
    it "8th move" $ move (Recipes (fromList "3710") '1' (fromList "012") '4' (fromList "51"))
                      `shouldBe` (Recipes (fromList "3") '7' (fromList "1010") '1' (fromList "24515"))
    it "9th move" $ move (Recipes (fromList "3") '7' (fromList "1010") '1' (fromList "24515"))
                      `shouldBe` (Recipes (fromList "37101012") '4' Empty '5' (fromList "158"))
    it "10th move" $ move (Recipes (fromList "37101012") '4' Empty '5' (fromList "158"))
                      `shouldBe` (Recipes (fromList "3") '7' (fromList "10101245158") '9' Empty)
    it "11th move" $ move (Recipes (fromList "3") '7' (fromList "10101245158") '9' Empty)
                      `shouldBe` (Recipes (fromList "3710101") '2' (fromList "4") '5' (fromList "158916"))
    it "12th move" $ move (Recipes (fromList "3710101") '2' (fromList "4") '5' (fromList "158916"))
                      `shouldBe` (Recipes (fromList "3710101245") '1' (fromList "5891") '6' (fromList "7"))
    it "13th move" $ move (Recipes (fromList "3710101245") '1' (fromList "5891") '6' (fromList "7"))
                      `shouldBe` (Recipes (fromList "3710") '1' (fromList "0124515") '8' (fromList "91677"))
    it "14th move" $ move (Recipes (fromList "3710") '1' (fromList "0124515") '8' (fromList "91677"))
                      `shouldBe` (Recipes (fromList "37") '1' (fromList "010") '1' (fromList "245158916779"))
    it "15th move" $ move (Recipes (fromList "37") '1' (fromList "010") '1' (fromList "245158916779"))
                      `shouldBe` (Recipes (fromList "3710") '1' (fromList "012") '4' (fromList "51589167792"))
    it "16th move" $ move (Recipes (fromList "3710") '1' (fromList "012") '4' (fromList "51589167792"))
                      `shouldBe` (Recipes (fromList "371010") '1' (fromList "245158") '9' (fromList "1677925"))
    it "nth move" $ move (Recipes (fromList "37101012451589167792510") '7' (fromList "108510111078111982") '4' (fromList "16"))
                      `shouldBe` (Recipes Empty '3' (fromList "710101245158916779251071085101") '1' (fromList "107811198241611"))

    it "make 5 recipes"  $ make 5 `shouldBe` "0124515891"
    it "make 9 recipes"  $ make 9 `shouldBe` "5158916779"
    it "make 18 recipes"  $ make 18 `shouldBe` "9251071085"
    it "make 38 recipes"  $ make 38 `shouldBe` "1982416114"
    it "make 2018 recipes"  $ make 2018 `shouldBe` "5941429882"
    it "make 030121 recipes"  $ make 030121 `shouldBe` "5101271252"

    it "search 01245 in recipes"  $ search (fromList "01245") `shouldBe` 5
    it "search 51589 in recipes"  $ search (fromList "51589") `shouldBe` 9
    it "search 92510 in recipes"  $ search (fromList "92510") `shouldBe` 18
    it "search 59414 in recipes"  $ search (fromList "59414") `shouldBe` 2018
    it "search 030121 in recipes"  $ search (fromList "030121") `shouldBe` 20287556
