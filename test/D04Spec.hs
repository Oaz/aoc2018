module D04Spec (spec) where

import Test.Hspec
import D04
import Text.Parsec
import Data.Time
import Data.List

spec :: Spec
spec = do

  let g1 = "[1518-11-04 00:00] Guard #10 begins shift\n[1518-11-04 00:05] falls asleep\n[1518-11-04 00:08] wakes up\n"
  let g2 = "[1518-11-03 23:58] Guard #15 begins shift\n[1518-11-04 00:11] falls asleep\n[1518-11-04 00:22] wakes up\n[1518-11-04 00:28] falls asleep\n[1518-11-04 00:42] wakes up\n"
  let g3 = g1++g2
  let g4 = "[1518-11-05 23:58] Guard #15 begins shift\n[1518-11-06 00:21] falls asleep\n[1518-11-06 00:24] wakes up\n"
  let g5 = g3++g4

  describe "parsing" $ do
    let today = fromGregorian 1518 11 4
    it "time" $ runParser logTime emptyLog "" "[1518-11-04 13:46]" `shouldBe` (Right $ LocalTime today (TimeOfDay 13 46 0))
    it "guard ID" $ runParser beginsShift emptyLog "" " Guard #10 begins shift\n" `shouldBe` Right 10
    it "log" $ runParser parseLog emptyLog "" g1 `shouldBe` Right [Sleep 10 today m | m <- [5..7]]
    it "log again" $ runParser parseLog emptyLog "" g3 `shouldBe` Right ([Sleep 10 today m | m <- [5..7]]++[Sleep 15 today m | m <- [11..21]]++[Sleep 15 today m | m <- [28..41]])
 
  describe "strategy 1" $ do
    let (Right sleeps) = runParser parseLog emptyLog "" g5
    it "select minute for guard" $ mostSleepingGuardMinute sleeps `shouldBe` (15,21)
    it "submits answer" $ do input <- readFile "test/D04.txt"; (mostSleepingGuardMinute $ gatherSleeps input) `shouldBe` (641,41)
    
  describe "strategy 2" $ do
    let (Right sleeps) = runParser parseLog emptyLog "" g5
    it "select minute for guard" $ mostFrequentlyAsleepGuardMinute sleeps `shouldBe` (15,21)
    it "submits answer" $ do input <- readFile "test/D04.txt"; (mostFrequentlyAsleepGuardMinute $ gatherSleeps input) `shouldBe` (1973,37)
  

  
