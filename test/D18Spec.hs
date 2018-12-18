module D18Spec (spec) where

import Test.Hspec
import D18
import qualified Data.HashMap.Strict as M

area = mkArea . parseInput

spec :: Spec
spec = do

  describe "parse input" $ do
    it "simple" $ parseInput "abc\n\
                             \def\n\
                             \ghi\n" `shouldBe` [(0:+0,'a')
                                                ,(1:+0,'b')
                                                ,(2:+0,'c')
                                                ,(0:+1,'d')
                                                ,(1:+1,'e')
                                                ,(2:+1,'f')
                                                ,(0:+2,'g')
                                                ,(1:+2,'h')
                                                ,(2:+2,'i')]

    it "area" $ area  ".#|\n\
                      \##.\n\
                      \||.\n" `shouldBe`  M.fromList [(0:+0,Glade)
                                                      ,(1:+0,Lumber)
                                                      ,(2:+0,Trees)
                                                      ,(0:+1,Lumber)
                                                      ,(1:+1,Lumber)
                                                      ,(2:+1,Glade)
                                                      ,(0:+2,Trees)
                                                      ,(1:+2,Trees)
                                                      ,(2:+2,Glade)]

  describe "lumber collection" $ do

    let a0 = area ".#.#...|#.\n\
                  \.....#|##|\n\
                  \.|..|...#.\n\
                  \..|#.....#\n\
                  \#.#|||#|#|\n\
                  \...#.||...\n\
                  \.|....|...\n\
                  \||...#|.#|\n\
                  \|.||||..|.\n\
                  \...#.|..|.\n"
                              
    let a1 = area ".......##.\n\
                  \......|###\n\
                  \.|..|...#.\n\
                  \..|#||...#\n\
                  \..##||.|#|\n\
                  \...#||||..\n\
                  \||...|||..\n\
                  \|||||.||.|\n\
                  \||||||||||\n\
                  \....||..|.\n"

    let a10 = area  ".||##.....\n\
                    \||###.....\n\
                    \||##......\n\
                    \|##.....##\n\
                    \|##.....##\n\
                    \|##....##|\n\
                    \||##.####|\n\
                    \||#####|||\n\
                    \||||#|||||\n\
                    \||||||||||\n"
    
    it "step 1" $ changeArea a0 `shouldBe` a1
    it "step 10" $ processArea 10 a0 `shouldBe` a10
    it "count" $ areaValue a10 `shouldBe` (37,31,1147)

    it "part 1" $ do
      input <- readFile "test/D18.txt";
      areaValue (processArea 10 $ area input) `shouldBe` (1025,622,637550)


