module D15Spec (spec) where

import Test.Hspec
import D15
import qualified Data.HashMap.Strict as M


setHP :: Coordinates -> Int -> Round -> Round
setHP p v (Round n bt sf w) = Round n (fst $ hitWarrior p (\_ _ -> v) bt) sf w

replay :: Int -> Round -> Round
replay n firstRound = head $ drop n $ iterate (playRound standardRules) firstRound

standardRules :: Rules
standardRules = Rules standardHitter

playStandardGame :: String -> (Int,Int,Int)
playStandardGame = playGame standardRules

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

  let bt1 = mkBattlefield 300 $ parseInput  "#######\n\
                                            \#E..G.#\n\ 
                                            \#...#.#\n\ 
                                            \#.G.#G#\n\ 
                                            \#######\n"
  
  describe "parse game" $ do
    it "bt1" $ bt1 `shouldBe` M.fromList  [(1:+1, Warrior $ Elf 300)
                                          ,(2:+1, Path)
                                          ,(3:+1, Path)
                                          ,(4:+1, Warrior $ Goblin 300)
                                          ,(5:+1, Path)
                                          ,(1:+2, Path)
                                          ,(2:+2, Path)
                                          ,(3:+2, Path)
                                          ,(5:+2, Path)
                                          ,(1:+3, Path)
                                          ,(2:+3, Warrior $ Goblin 300)
                                          ,(3:+3, Path)
                                          ,(5:+3, Warrior $ Goblin 300)]
                    
  describe "find warriors" $ do
    it "warriors" $ findWarriors bt1 `shouldBe` [1:+1, 4:+1, 2:+3, 5:+3]
                                        
  describe "lee algorithm" $ do
    it "example 1" $ leeAlgorithm (5:+5) (\(x:+y) -> (x==3)) (\(x:+y) -> (y==5))
                          `shouldBe` [4 :+ 5,3 :+ 5]
    it "example 2" $ leeAlgorithm (0:+0) (\c -> (abs c) > 1) (\(x:+y) -> abs(y)<2)
                          `shouldBe` [0 :+ (-1),(-1) :+ (-1)]
  

  describe "find enemy" $ do
    it "goblin 1" $ findEnemy bt1 (4:+1) `shouldBe` [3:+1, 2:+1, 1:+1]
    it "goblin 2" $ findEnemy bt1 (2:+3) `shouldBe` [2:+2, 2:+1, 1:+1]
    it "goblin 3 is blocked" $ findEnemy bt1 (5:+3) `shouldBe` []
    it "elf" $ findEnemy bt1 (1:+1) `shouldBe` [2:+1, 3:+1, 4:+1]

    let tricky = mkBattlefield 300 $ parseInput "######\n\
                                                \#.E.G#\n\ 
                                                \#G...#\n\ 
                                                \######\n"
    it "tricky" $ findEnemy tricky (2:+1) `shouldBe` [1:+1, 1:+2]

    let shortestPath = mkBattlefield 300 $ parseInput "#######\n\
                                                      \#.E...#\n\
                                                      \#.....#\n\
                                                      \#...G.#\n\
                                                      \#######\n"
    it "shortestPath" $ findEnemy shortestPath (2:+1) `shouldBe` [3:+1, 4:+1, 4:+2, 4:+3]


  describe "find weakest" $ do
    let weakEnemies = M.fromList  [(1:+1, Warrior $ Elf 300)
                                  ,(0:+1, Warrior $ Goblin 15)
                                  ,(2:+1, Warrior $ Goblin 8)
                                  ,(1:+0, Path)
                                  ,(1:+2, Warrior $ Elf 3)]
    it "weakest" $ findWeakest weakEnemies (1:+1) `shouldBe` Just (2:+1)
  
  let btA0 = mkBattlefield 200 $ parseInput "#########\n\
                                            \#G..G..G#\n\
                                            \#.......#\n\
                                            \#.......#\n\
                                            \#G..E..G#\n\
                                            \#.......#\n\
                                            \#.......#\n\
                                            \#G..G..G#\n\
                                            \#########\n"
  let rA0 = startGame btA0

  let btA1 = mkBattlefield 200 $ parseInput "#########\n\
                                            \#.G...G.#\n\
                                            \#...G...#\n\
                                            \#...E..G#\n\
                                            \#.G.....#\n\
                                            \#.......#\n\
                                            \#G..G..G#\n\
                                            \#.......#\n\
                                            \#########\n"
  let rA1 = setHP (4:+2) 197
          $ (startGame btA1) { roundID = 1 }

  let btA2 = mkBattlefield 200 $ parseInput "#########\n\
                                            \#..G.G..#\n\
                                            \#...G...#\n\
                                            \#.G.E.G.#\n\
                                            \#.......#\n\
                                            \#G..G..G#\n\
                                            \#.......#\n\
                                            \#.......#\n\
                                            \#########\n"
  let rA2 = setHP (4:+2) 194
          $ setHP (4:+3) 197
          $ (startGame btA2) { roundID = 2 }

  let btA3 = mkBattlefield 200 $ parseInput "#########\n\
                                            \#.......#\n\
                                            \#..GGG..#\n\
                                            \#..GEG..#\n\
                                            \#G..G...#\n\
                                            \#......G#\n\
                                            \#.......#\n\
                                            \#.......#\n\
                                            \#########\n"
  let rA3 = setHP (4:+2) 191
          $ setHP (4:+3) 185
          $ (startGame btA3) { roundID = 3 }




  describe "play turn" $ do
    it "round A1" $ playRound standardRules rA0 `shouldBe` rA1 
    it "round A2" $ playRound standardRules rA1 `shouldBe` rA2 
    it "round A3" $ playRound standardRules rA2 `shouldBe` rA3 

    let rB0 = startGame $ mkBattlefield 200 $ parseInput  "#######\n\
                                                          \#.G...#\n\
                                                          \#...EG#\n\
                                                          \#.#.#G#\n\
                                                          \#..G#E#\n\
                                                          \#.....#\n\
                                                          \#######\n"
    it "round B1" $ replay 1 rB0 `shouldBe` 
                      ( setHP (4:+2) 197
                      $ setHP (5:+2) 197
                      $ setHP (5:+3) 197
                      $ setHP (5:+4) 197
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#..G..#\n\
                                        \#...EG#\n\
                                        \#.#G#G#\n\
                                        \#...#E#\n\
                                        \#.....#\n\
                                        \#######\n") { roundID = 1
                                                    , wasFightingInPreviousTurn = True } )

    it "round B2" $ replay 2 rB0 `shouldBe` 
                      ( setHP (4:+2) 188
                      $ setHP (5:+2) 194
                      $ setHP (5:+3) 194
                      $ setHP (5:+4) 194
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#...G.#\n\
                                        \#..GEG#\n\
                                        \#.#.#G#\n\
                                        \#...#E#\n\
                                        \#.....#\n\
                                        \#######\n") { roundID = 2
                                                    , wasFightingInPreviousTurn = True } )

    it "round B23" $ replay 23 rB0 `shouldBe`
                      ( setHP (5:+2) 131
                      $ setHP (5:+3) 131
                      $ setHP (5:+4) 131
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#...G.#\n\
                                        \#..G.G#\n\
                                        \#.#.#G#\n\
                                        \#...#E#\n\
                                        \#.....#\n\
                                        \#######\n") { roundID = 23
                                                    , wasFightingInPreviousTurn = True } )

    it "round B24" $ replay 24 rB0 `shouldBe`
                      ( setHP (4:+2) 131
                      $ setHP (5:+3) 128
                      $ setHP (5:+4) 128
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#..G..#\n\
                                        \#...G.#\n\
                                        \#.#G#G#\n\
                                        \#...#E#\n\
                                        \#.....#\n\
                                        \#######\n") { roundID = 24
                                                    , wasFightingInPreviousTurn = True } )

    it "round B25" $ replay 25 rB0 `shouldBe`
                      ( setHP (3:+2) 131
                      $ setHP (5:+3) 125
                      $ setHP (5:+4) 125
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#.G...#\n\
                                        \#..G..#\n\
                                        \#.#.#G#\n\
                                        \#..G#E#\n\
                                        \#.....#\n\
                                        \#######\n") { roundID = 25
                                                    , wasFightingInPreviousTurn = True } )

    it "round B28" $ replay 28 rB0 `shouldBe`
                      ( setHP (2:+2) 131
                      $ setHP (5:+3) 116
                      $ setHP (5:+4) 113
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#G....#\n\
                                        \#.G...#\n\
                                        \#.#.#G#\n\
                                        \#...#E#\n\
                                        \#....G#\n\
                                        \#######\n") { roundID = 28
                                                    , wasFightingInPreviousTurn = True } )

    it "round B47" $ replay 47 rB0 `shouldBe`
                      ( setHP (2:+2) 131
                      $ setHP (5:+3) 59
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#G....#\n\
                                        \#.G...#\n\
                                        \#.#.#G#\n\
                                        \#...#.#\n\
                                        \#....G#\n\
                                        \#######\n") { roundID = 47
                                                    , wasFightingInPreviousTurn = True } )

    it "round B48" $ replay 48 rB0 `shouldBe`
                      ( setHP (2:+2) 131
                      $ setHP (5:+3) 59
                      $ (startGame $ mkBattlefield 200
                          $ parseInput  "#######\n\
                                        \#G....#\n\
                                        \#.G...#\n\
                                        \#.#.#G#\n\
                                        \#...#.#\n\
                                        \#....G#\n\
                                        \#######\n") { roundID = 48 } )

    let rC0 = setHP (3:+2) 2
              $ startGame
              $ mkBattlefield 200
              $ parseInput  "######\n\
                            \###G.#\n\
                            \#.GE.#\n\
                            \#..G.#\n\
                            \####.#\n\
                            \####.#\n\
                            \####E#\n\
                            \######\n"
    it "round C0" $ replay 1 rC0 `shouldBe` 
                    (startGame $ mkBattlefield 200
                        $ parseInput  "######\n\
                                      \###G.#\n\
                                      \#..G.#\n\
                                      \#...G#\n\
                                      \####.#\n\
                                      \####E#\n\
                                      \####.#\n\
                                      \######\n") { roundID = 1 }

  describe "play game" $ do
    it "example 0" $ playStandardGame "#######\n\
                                      \#.G...#\n\
                                      \#...EG#\n\
                                      \#.#.#G#\n\
                                      \#..G#E#\n\
                                      \#.....#\n\
                                      \#######" `shouldBe` (47,590,27730) 

    it "example 1" $ playStandardGame "#######\n\
                                      \#G..#E#\n\
                                      \#E#E.E#\n\
                                      \#G.##.#\n\
                                      \#...#E#\n\
                                      \#...E.#\n\
                                      \#######" `shouldBe` (37,982,36334) 
                                           
    it "example 2" $ playStandardGame "#######\n\
                                      \#E..EG#\n\
                                      \#.#G.E#\n\
                                      \#E.##E#\n\
                                      \#G..#.#\n\
                                      \#..E#.#\n\
                                      \#######" `shouldBe` (46,859,39514) 
                                           
    it "example 3" $ playStandardGame "#######\n\
                                      \#E.G#.#\n\
                                      \#.#G..#\n\
                                      \#G.#.G#\n\
                                      \#G..#.#\n\
                                      \#...E.#\n\
                                      \#######" `shouldBe` (35,793,27755) 
                                           
    it "example 4" $ playStandardGame "#######\n\
                                      \#.E...#\n\
                                      \#.#..G#\n\
                                      \#.###.#\n\
                                      \#E#G#G#\n\
                                      \#...#G#\n\
                                      \#######" `shouldBe` (54,536,28944) 
                                           
    it "example 5" $ playStandardGame "#########\n\
                                      \#G......#\n\
                                      \#.E.#...#\n\
                                      \#..##..G#\n\
                                      \#...##..#\n\
                                      \#...#...#\n\
                                      \#.G...G.#\n\
                                      \#.....G.#\n\
                                      \#########\n" `shouldBe` (20,937,18740)

  describe "my input" $ do

    it "submits answer" $ do input <- readFile "test/D15.txt"; (playStandardGame input) `shouldBe` (67,2922,195774)

  describe "part 2" $ do

    it "example 1" $ findAttackPower  "#######\n\
                                      \#.G...#\n\
                                      \#...EG#\n\
                                      \#.#.#G#\n\
                                      \#..G#E#\n\
                                      \#.....#\n\
                                      \#######" `shouldBe` (15,(29,172,4988))

    it "example 2" $ findAttackPower  "#######\n\
                                      \#E..EG#\n\
                                      \#.#G.E#\n\
                                      \#E.##E#\n\
                                      \#G..#.#\n\
                                      \#..E#.#\n\
                                      \#######" `shouldBe` (4,(33,948,31284))

    it "example 3" $ findAttackPower  "#######\n\
                                      \#E.G#.#\n\
                                      \#.#G..#\n\
                                      \#G.#.G#\n\
                                      \#G..#.#\n\
                                      \#...E.#\n\
                                      \#######" `shouldBe` (15,(37,94,3478))

    it "example 4" $ findAttackPower  "#######\n\
                                      \#.E...#\n\
                                      \#.#..G#\n\
                                      \#.###.#\n\
                                      \#E#G#G#\n\
                                      \#...#G#\n\
                                      \#######" `shouldBe` (12,(39,166,6474))

    it "example 5" $ findAttackPower  "#########\n\
                                      \#G......#\n\
                                      \#.E.#...#\n\
                                      \#..##..G#\n\
                                      \#...##..#\n\
                                      \#...#...#\n\
                                      \#.G...G.#\n\
                                      \#.....G.#\n\
                                      \#########\n" `shouldBe` (34,(30,38,1140))

    it "submits answer" $ do input <- readFile "test/D15.txt"; (findAttackPower input) `shouldBe` (34,(24,1553,37272))

