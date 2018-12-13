module D13Spec (spec) where

import Test.Hspec
import D13
import Data.List
import qualified Data.HashMap.Strict as M
import Data.Ord

spec :: Spec
spec = do

  describe "parse input" $ do
    it "simple" $ parseInput "abc\n\
                             \d f\n\
                             \ghi\n" `shouldBe` [(0:+0,'a')
                                                ,(1:+0,'b')
                                                ,(2:+0,'c')
                                                ,(0:+1,'d')
                                                ,(2:+1,'f')
                                                ,(0:+2,'g')
                                                ,(1:+2,'h')
                                                ,(2:+2,'i')]
  let groundDef = [Road (0:+0) CurveSlash
                  ,Road (1:+0) StraightEW
                  ,Road (2:+0) StraightEW
                  ,Road (3:+0) StraightEW
                  ,Road (4:+0) CurveAnti
                  ,Road (0:+1) StraightNS
                  ,Road (2:+1) CurveSlash
                  ,Road (3:+1) StraightEW
                  ,Road (4:+1) Cross
                  ,Road (5:+1) StraightEW
                  ,Road (6:+1) CurveAnti
                  ,Road (0:+2) CurveAnti
                  ,Road (1:+2) StraightEW
                  ,Road (2:+2) Cross
                  ,Road (3:+2) StraightEW
                  ,Road (4:+2) CurveSlash
                  ,Road (6:+2) StraightNS
                  ,Road (2:+3) StraightNS
                  ,Road (6:+3) StraightNS
                  ,Road (2:+4) CurveAnti
                  ,Road (3:+4) StraightEW
                  ,Road (4:+4) StraightEW
                  ,Road (5:+4) StraightEW
                  ,Road (6:+4) CurveSlash]
  let ground = M.fromList $ map (\r -> (situation r,r)) groundDef
  let cart1 = Cart (2:+0) East GoLeft
  let cart2 = Cart (1:+2) West GoLeft
  let cart3 = Cart (6:+2) South GoLeft
  let cart4 = Cart (2:+3) North GoLeft
  let fleet = cartsToFleet [cart1,cart2,cart3,cart4]

  describe "parse world" $ do

    it "simple" $ parseWorld  "/->-\\ \n\
                              \| /-+-\\\n\
                             \\\<+-/ v\n\
                              \  ^   |\n\
                             \  \\---/\n" `shouldBe` (ground,fleet)
  
  describe "move carts" $ do
    it "goes straight" $ nextCart ground (Cart (2:+0) East GoLeft) `shouldBe` (Cart (3:+0) East GoLeft)
    it "turns" $ nextCart ground (Cart (1:+2) West GoLeft) `shouldBe` (Cart (0:+2) North GoLeft)
    it "crosses" $ nextCart ground (Cart (2:+3) North GoLeft) `shouldBe` (Cart (2:+2) West GoStraight)
    it "crosses again" $ nextCart ground (Cart (2:+3) North GoStraight) `shouldBe` (Cart (2:+2) North GoRight)
    it "crosses still" $ nextCart ground (Cart (2:+3) North GoRight) `shouldBe` (Cart (2:+2) East GoLeft)

  describe "forward fleet" $ do
    let fx (Fleet a _ p) = (sortBy (comparing position) $ M.elems a,p)
    it "no crash" $ (fx $ forward ground fleet) `shouldBe` ([Cart (3:+0) East GoLeft,cart2,cart3,cart4],Nothing)
    it "crash" $ (fx $ forward ground (cartsToFleet [Cart (0:+2) East GoLeft,cart2,cart3,cart4])) `shouldBe` ([cart3,cart4], Just $ 1:+2)

  describe "find crash" $ do
    it "example" $ findCrash (ground,fleet) `shouldBe` 3:+1
    it "example 2" $ (findCrash $ parseWorld "/->-\\         \n\       
                                                 \|   |  /----\\  \n\
                                                 \| /-+--+-\\  |  \n\
                                                 \| | |  | v  |  \n\
                                                \\\-+-/  \\-+--/  \n\
                                                \  \\------/     \n")   `shouldBe` 7:+3
    it "submits answer" $ do input <- readFile "test/D13.txt"; (findCrash $ parseWorld input) `shouldBe` 64:+57

  describe "avoid crash" $ do
    it "example" $ (avoidCrash $ parseWorld "/>-<\\   \n\
                                            \|   |   \n\
                                            \| /<+-\\ \n\
                                            \| | | v \n\
                                           \\\>+</ | \n\
                                            \  |   ^ \n\
                                           \  \\<->/  \n") `shouldBe` 6:+5
    it "submits answer" $ do input <- readFile "test/D13.txt"; (avoidCrash $ parseWorld input) `shouldBe` 136:+8
 

  