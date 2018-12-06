module D06Spec (spec) where

import Test.Hspec
import D06

spec :: Spec
spec = do

    describe "manhattan" $ do
      it "distance to origin" $ manhattan (2 :+ 3) `shouldBe` 5
      it "other distance to origin" $ manhattan (4 :+ (-5)) `shouldBe` 9
      it "distance between 2 coordinates" $ manhattan ((2 :+ 3) - (4 :+ (-5))) `shouldBe` 10

    describe "quadrant" $ do
      it "x > 0, |y| <= |x|" $ quadrant (3 :+ 1) `shouldBe` [True,False,False,False]
      it "x < 0, |y| <= |x|" $ quadrant ((-3) :+ 1) `shouldBe` [False,False,True,False]
      it "y > 0, |x| <= |y|" $ quadrant (1 :+ 3) `shouldBe` [False,True,False,False]
      it "y < 0, |x| <= |y|" $ quadrant (1 :+ (-3)) `shouldBe` [False,False,False,True]
      it "x = y > 0" $ quadrant (3 :+ 3) `shouldBe` [True,True,False,False]
      it "-x = y > 0" $ quadrant ((-3) :+ 3) `shouldBe` [False,True,True,False]
      it "x = y < 0" $ quadrant ((-3) :+ (-3)) `shouldBe` [False,False,True,True]
      it "-x = y < 0" $ quadrant (3 :+ (-3)) `shouldBe` [True,False,False,True]

    let a = 1:+1
    let b = 1:+6
    let c = 8:+3
    let d = 3:+4
    let e = 5:+5
    let f = 8:+9
    let points = [a,b,c,d,e,f]

    describe "core" $ do
        it "empty" $ core [] `shouldBe` []
        let eastPoints = [4:+(-2),5:+1,3:+2,8:+0]
        it "east" $ core eastPoints `shouldBe` [3:+2]
        let northPoints = [3:+4,(-2):+3,1:+5,0:+8]
        it "north" $ core northPoints `shouldBe` [(-2):+3]
        let westPoints = [(-4):+(-2),(-3):+2,(-5):+1,(-8):+0]
        it "west" $ core westPoints `shouldBe` [(-3):+2]
        let southPoints = [3:+(-4),(-2):+(-3),1:+(-5),0:+(-8)]
        it "south" $ core southPoints `shouldBe` [(-2):+(-3)]
        it "north and east" $ core (northPoints++eastPoints) `shouldBe` [3:+2,(-2):+3]
        it "all points" $ core (northPoints++eastPoints++southPoints++westPoints) `shouldBe` [3:+2,(-2):+3,(-3):+2,(-2):+(-3)]
        it "A" $ core (translate (-a) points) `shouldBe` (translate (-a) [e, b])
        it "B" $ core (translate (-b) points) `shouldBe` (translate (-b) [d])
        it "C" $ core (translate (-c) points) `shouldBe` (translate (-c) [f, e])
        it "D" $ core (translate (-d) points) `shouldBe` (translate (-d) [e, b, a])
        it "E" $ core (translate (-e) points) `shouldBe` (translate (-e) [c, f, d, a])
        it "F" $ core (translate (-f) points) `shouldBe` (translate (-f) [b, c])

    describe "area" $ do
      it "A" $ area points a `shouldBe` Nothing
      it "B" $ area points b `shouldBe` Nothing
      it "C" $ area points c `shouldBe` Nothing
      it "D" $ area points d `shouldBe` Just 9
      it "E" $ area points e `shouldBe` Just 17
      it "F" $ area points f `shouldBe` Nothing
      it "max" $ maximalArea points `shouldBe` Just 17
      it "submits answer" $ do input <- readFile "test/D06.txt"; (maximalArea $ mkCoordinates input) `shouldBe` Just 3449
      let h = 4:+3
      it "H" $ distanceBelow 32 points h `shouldBe` Just 30
      it "max2" $ maximalArea2 32 points `shouldBe` 16
      it "submits answer 2" $ do input <- readFile "test/D06.txt"; (maximalArea2 10000 $ mkCoordinates input) `shouldBe` 44868

