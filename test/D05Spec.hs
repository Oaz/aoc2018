module D05Spec (spec) where

import Test.Hspec
import D05

spec :: Spec
spec = do

    describe "chain reaction" $ do
        it "couple of opposites" $ chainReaction "aA" `shouldBe` ""
        it "couple of opposites inside couple of opposites" $ chainReaction "abBA" `shouldBe` ""
        it "blocked couples of opposites" $ chainReaction "abAB" `shouldBe` "abAB"
        it "larger example" $ chainReaction "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"
        it "submits answer" $ do input <- readFile "test/D05.txt"; (length (chainReaction $ head $ words input)) `shouldBe` 10638

    describe "altered chain reaction" $ do
        it "remove a/A" $ alteredChainReaction 'a' "dabAcCaCBAcCcaDA" `shouldBe` "dbCBcD"
        it "remove b/B" $ alteredChainReaction 'b' "dabAcCaCBAcCcaDA" `shouldBe` "daCAcaDA"
        it "remove c/C" $ alteredChainReaction 'c' "dabAcCaCBAcCcaDA" `shouldBe` "daDA"
        it "remove d/D" $ alteredChainReaction 'd' "dabAcCaCBAcCcaDA" `shouldBe` "abCBAc"
        it "best" $ bestAlteredChainReactionSize "dabAcCaCBAcCcaDA" `shouldBe` 4
        it "submits answer" $ do input <- readFile "test/D05.txt"; (bestAlteredChainReactionSize $ head $ words input) `shouldBe` 4944

              
