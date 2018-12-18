module D18Spec (spec) where

import Test.Hspec
import D18
import Data.Matrix

spec :: Spec
spec = do

  describe "parse input" $ do

    it "area" $ mkArea  ".#|\n\
                        \##.\n\
                        \||.\n" `shouldBe` fromList 5 5 [Void,Void,Void,Void,Void
                                                        ,Void,Glade,Lumber,Trees,Void
                                                        ,Void,Lumber,Lumber,Glade,Void
                                                        ,Void,Trees,Trees,Glade,Void
                                                        ,Void,Void,Void,Void,Void]

  describe "lumber collection" $ do

    let a0 = mkArea ".#.#...|#.\n\
                    \.....#|##|\n\
                    \.|..|...#.\n\
                    \..|#.....#\n\
                    \#.#|||#|#|\n\
                    \...#.||...\n\
                    \.|....|...\n\
                    \||...#|.#|\n\
                    \|.||||..|.\n\
                    \...#.|..|.\n"
                              
    let a1 = mkArea ".......##.\n\
                    \......|###\n\
                    \.|..|...#.\n\
                    \..|#||...#\n\
                    \..##||.|#|\n\
                    \...#||||..\n\
                    \||...|||..\n\
                    \|||||.||.|\n\
                    \||||||||||\n\
                    \....||..|.\n"

    let a10 = mkArea  ".||##.....\n\
                      \||###.....\n\
                      \||##......\n\
                      \|##.....##\n\
                      \|##.....##\n\
                      \|##....##|\n\
                      \||##.####|\n\
                      \||#####|||\n\
                      \||||#|||||\n\
                      \||||||||||\n"
    
    it "vicinity 1" $ countVicinity a0 (10,3) `shouldBe` (AtLeastOne,AtLeastThree)
    it "vicinity 2" $ countVicinity a0 (7,3) `shouldBe` (AtLeastOne,Zero)

    it "step 1" $ changeArea a0 `shouldBe` a1
    it "step 10" $ processArea 10 a0 `shouldBe` a10
    it "count" $ areaValue a10 `shouldBe` (37,31,1147)

    it "part 1" $ do
      input <- readFile "test/D18.txt";
      areaValue (processArea 10 $ mkArea input) `shouldBe` (1025,622,637550)

    it "part 2" $ do
      input <- readFile "test/D18.txt";
      guessAreaValue (mkArea input) 1000 1000000000 `shouldBe` (605,333,201465)
  
  

