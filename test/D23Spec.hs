module D23Spec (spec) where

import Test.Hspec
import D23

spec :: Spec
spec = do

    let sampleBots =  [ Octahedron 4 $ Coords 0 0 0
                      , Octahedron 1 $ Coords 1 0 0
                      , Octahedron 3 $ Coords 4 0 0
                      , Octahedron 1 $ Coords 0 2 0
                      , Octahedron 3 $ Coords 0 5 0
                      , Octahedron 1 $ Coords 0 0 3
                      , Octahedron 1 $ Coords 1 1 1
                      , Octahedron 1 $ Coords 1 1 2
                      , Octahedron 1 $ Coords 1 3 1 ]
    describe "nanobots" $ do
      it "read from string" $ mkNanobots  "pos=<0,0,0>, r=4\n\
                                          \pos=<1,0,0>, r=1\n\
                                          \pos=<4,0,0>, r=3\n\
                                          \pos=<0,2,0>, r=1\n\
                                          \pos=<0,5,0>, r=3\n\
                                          \pos=<0,0,3>, r=1\n\
                                          \pos=<1,1,1>, r=1\n\
                                          \pos=<1,1,2>, r=1\n\
                                          \pos=<1,3,1>, r=1" `shouldBe` sampleBots

      it "counts in range" $ countInRangeOfStrongest sampleBots `shouldBe` 7

    describe "part 1" $ do
      it "submits answer" $ do input <- readFile "test/D23.txt"; (countInRangeOfStrongest $ mkNanobots input) `shouldBe` 383

    let sampleBots2 = [ Octahedron 2 $ Coords 10 12 12
                      , Octahedron 2 $ Coords 12 14 12
                      , Octahedron 4 $ Coords 16 12 12
                      , Octahedron 6 $ Coords 14 14 14
                      , Octahedron 200 $ Coords 50 50 50
                      , Octahedron 5 $ Coords 10 10 10 ]
    describe "nanobots 2" $ do

      it "best coords" $ bestCoords sampleBots2 `shouldBe` Win (Coords 12 12 12) 5 36
   

    describe "part 2" $ do

      it "best coords" $ do
        input <- readFile "test/D23.txt"
        let bots = mkNanobots input
        bestCoords bots `shouldBe` Win (Coords 35689633 20484373 44300020) 980 100474026
