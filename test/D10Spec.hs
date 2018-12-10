module D10Spec (spec) where

import Test.Hspec
import D10

spec :: Spec
spec = do
    let sampleInput = [ "position=< 9,  1> velocity=< 0,  2>"
                      , "position=< 7,  0> velocity=<-1,  0>"
                      , "position=< 3, -2> velocity=<-1,  1>"
                      , "position=< 6, 10> velocity=<-2, -1>"
                      , "position=< 2, -4> velocity=< 2,  2>"
                      , "position=<-6, 10> velocity=< 2, -2>"
                      , "position=< 1,  8> velocity=< 1, -1>"
                      , "position=< 1,  7> velocity=< 1,  0>"
                      , "position=<-3, 11> velocity=< 1, -2>"
                      , "position=< 7,  6> velocity=<-1, -1>"
                      , "position=<-2,  3> velocity=< 1,  0>"
                      , "position=<-4,  3> velocity=< 2,  0>"
                      , "position=<10, -3> velocity=<-1,  1>"
                      , "position=< 5, 11> velocity=< 1, -2>"
                      , "position=< 4,  7> velocity=< 0, -1>"
                      , "position=< 8, -2> velocity=< 0,  1>"
                      , "position=<15,  0> velocity=<-2,  0>"
                      , "position=< 1,  6> velocity=< 1,  0>"
                      , "position=< 8,  9> velocity=< 0, -1>"
                      , "position=< 3,  3> velocity=<-1,  1>"
                      , "position=< 0,  5> velocity=< 0, -1>"
                      , "position=<-2,  2> velocity=< 2,  0>"
                      , "position=< 5, -2> velocity=< 1,  2>"
                      , "position=< 1,  4> velocity=< 2,  1>"
                      , "position=<-2,  7> velocity=< 2, -2>"
                      , "position=< 3,  6> velocity=<-1, -1>"
                      , "position=< 5,  0> velocity=< 1,  0>"
                      , "position=<-6,  0> velocity=< 2,  0>"
                      , "position=< 5,  9> velocity=< 1, -2>"
                      , "position=<14,  7> velocity=<-2,  0>"
                      , "position=<-3,  6> velocity=< 2, -1>" ]
    
    let sky = mkSky $ unlines sampleInput
    let (Sky _ points) = sky

    describe "parse" $ do
      it "build sky"  $ (head points) `shouldBe` (9:+1,0:+2)
      it "fully build sky"  $ (last points) `shouldBe` ((-3):+6,2:+(-1))

    describe "find message" $ do
      let msg = findMessage 9 sky
      it "sample stream"  $ show (findMessage 9 sky) 
                              `shouldBe`  "3\n\
                                          \#...#..###\n\
                                          \#...#...#.\n\
                                          \#...#...#.\n\
                                          \#####...#.\n\
                                          \#...#...#.\n\
                                          \#...#...#.\n\
                                          \#...#...#.\n\
                                          \#...#..###\n"
      it "submits answer" $ do
        input <- readFile "test/D10.txt"
        show (findMessage 10 (mkSky input))
          `shouldBe`  "10942\n\
                      \#####...#####.....##....#....#..######..#.........##.....####.\n\
                      \#....#..#....#...#..#...##...#.......#..#........#..#...#....#\n\
                      \#....#..#....#..#....#..##...#.......#..#.......#....#..#.....\n\
                      \#....#..#....#..#....#..#.#..#......#...#.......#....#..#.....\n\
                      \#####...#####...#....#..#.#..#.....#....#.......#....#..#.....\n\
                      \#..#....#..#....######..#..#.#....#.....#.......######..#.....\n\
                      \#...#...#...#...#....#..#..#.#...#......#.......#....#..#.....\n\
                      \#...#...#...#...#....#..#...##..#.......#.......#....#..#.....\n\
                      \#....#..#....#..#....#..#...##..#.......#.......#....#..#....#\n\
                      \#....#..#....#..#....#..#....#..######..######..#....#...####.\n"

