module D17Spec (spec) where

import Test.Hspec
import D17

spec :: Spec
spec = do

  let u = Underground (500:+0) 13 (495:+1,506:+13)
                      $ groupClays  [ [495:+y | y<-[2..7]]
                                    , [x:+7 | x<-[495..501]]
                                    , [501:+y | y<-[3..7]]
                                    , [498:+y | y<-[2..4]]
                                    , [506:+y | y<-[1..2]]
                                    , [498:+y | y<-[10..13]]
                                    , [504:+y | y<-[10..13]]
                                    , [x:+13 | x<-[498..504]] ]

  let ux = mkUnderground  "x=496, y=5..15\n\
                          \x=512, y=6..15\n\
                          \y=15, x=497..511\n\
                          \x=501, y=3..10\n\
                          \x=506, y=3..10\n\
                          \y=10, x=502..505\n"

  let uy = mkUnderground  "x=493, y=5..15\n\
                          \x=509, y=6..15\n\
                          \y=15, x=494..508\n\
                          \x=515, y=3..10\n\
                          \x=498, y=3..10\n\
                          \x=503, y=3..10\n\
                          \y=10, x=499..502\n"

  let uz = mkUnderground  "x=496, y=5..15\n\
                          \x=512, y=6..15\n\
                          \y=15, x=497..511\n\
                          \x=500, y=3..10\n\
                          \x=506, y=3..10\n\
                          \y=10, x=501..505\n"
                          
  describe "parse input" $ do
    it "horizontal clays" $ readClays "y=938, x=593..595" `shouldBe` [x:+938 | x<-[593..595]]
    it "vertical clays" $ readClays "x=495, y=2..7" `shouldBe` [495:+y | y<-[2..7]]

    it "underground" $ mkUnderground  "x=495, y=2..7\n\
                                      \y=7, x=495..501\n\
                                      \x=501, y=3..7\n\
                                      \x=498, y=2..4\n\
                                      \x=506, y=1..2\n\
                                      \x=498, y=10..13\n\
                                      \x=504, y=10..13\n\
                                      \y=13, x=498..504\n" `shouldBe` u

  describe "browse underground" $ do
    it "sand" $ geologyAt u (1500:+13) `shouldBe` Sand
    it "clay" $ geologyAt u (500:+7) `shouldBe` Clay
    it "flood below bottom" $ geologyAt u (510:+14) `shouldBe` Flood

  describe "moving water" $ do
    it "a1" $ move u (Water (500:+5) GoDown) `shouldBe` (Water (500:+6) GoDown)
    it "a2" $ move u (Water (500:+6) GoDown) `shouldBe` (Water (499:+6) GoLeft)
    it "a3" $ move u (Water (499:+6) GoLeft) `shouldBe` (Water (498:+6) GoLeft)
    it "a4" $ move u (Water (496:+6) GoLeft) `shouldBe` (Water (497:+6) (GoRight Clay))
    it "a5" $ move u (Water (497:+6) (GoRight Clay)) `shouldBe` (Water (498:+6) (GoRight Clay))
    it "a6" $ move u (Water (499:+6) (GoRight Clay)) `shouldBe` (Water (500:+6) (GoRight Clay))
    it "a7" $ move u (Water (500:+6) (GoRight Clay)) `shouldBe` (Water (500:+6) (StopAndFill Groundwater))
    it "a last" $ findLast u `shouldBe` (Water (500:+6) (StopAndFill Groundwater))

    let ub =  fillRow (Water (500:+6) (StopAndFill Groundwater)) u
    it "b1" $ move ub (Water (500:+5) GoDown) `shouldBe` (Water (499:+5) GoLeft)
    it "b2" $ move ub (Water (499:+5) GoLeft) `shouldBe` (Water (498:+5) GoLeft)
    it "b3" $ move ub (Water (498:+5) GoLeft) `shouldBe` (Water (497:+5) GoLeft)
    it "b4" $ move ub (Water (499:+5) (GoRight Clay)) `shouldBe` (Water (500:+5) (GoRight Clay))
    it "b5" $ move ub (Water (500:+5) (GoRight Clay)) `shouldBe` (Water (500:+5) (StopAndFill Groundwater))
    it "b last" $ findLast ub `shouldBe` (Water (500:+5) (StopAndFill Groundwater))

    let uc =  fillRow (Water (500:+5) (StopAndFill Groundwater)) ub
    it "c1" $ move uc (Water (500:+4) GoDown) `shouldBe` (Water (499:+4) GoLeft)
    it "c2" $ move uc (Water (499:+4) GoLeft) `shouldBe` (Water (500:+4) (GoRight Clay))
    it "c3" $ move uc (Water (500:+4) (GoRight Clay)) `shouldBe` (Water (500:+4) (StopAndFill Groundwater))
    it "c last" $ findLast uc `shouldBe` (Water (500:+4) (StopAndFill Groundwater))

    let ud =  fillRow (Water (500:+3) (StopAndFill Groundwater)) 
            $ fillRow (Water (500:+4) (StopAndFill Groundwater)) uc
    it "d1" $ move ud (Water (500:+2) GoDown) `shouldBe` (Water (499:+2) GoLeft)
    it "d2" $ move ud (Water (500:+2) (GoRight Clay)) `shouldBe` (Water (501:+2) (GoRight Clay))
    it "d3" $ move ud (Water (501:+2) (GoRight Clay)) `shouldBe` (Water (502:+2) (GoRight Clay))
    it "d4" $ move ud (Water (502:+2) (GoRight Clay)) `shouldBe` (Water (502:+3) GoDown)
    it "d5" $ move ud (Water (502:+12) GoDown) `shouldBe` (Water (501:+12) GoLeft)
    it "d6" $ move ud (Water (503:+12) (GoRight Clay)) `shouldBe` (Water (503:+12) (StopAndFill Groundwater))
    it "d last" $ findLast ud `shouldBe` (Water (503:+12) (StopAndFill Groundwater))

    let ue =  fillRow (Water (503:+10) (StopAndFill Groundwater)) 
            $ fillRow (Water (503:+11) (StopAndFill Groundwater)) 
            $ fillRow (Water (503:+12) (StopAndFill Groundwater)) ud
    it "e1" $ move ue (Water (502:+9) GoDown) `shouldBe` (Water (501:+9) GoLeft)
    it "e2" $ move ue (Water (498:+9) GoLeft) `shouldBe` (Water (497:+9) GoLeft)
    it "e3" $ move ue (Water (497:+9) GoLeft) `shouldBe` (Water (497:+10) GoDown)
    it "e4" $ move ue (Water (497:+12) GoDown) `shouldBe` (Water (497:+13) GoDown)
    it "e5" $ move ue (Water (497:+13) GoDown) `shouldBe` (Water (497:+13) (Stop Flood))
    it "e last" $ findLast ue `shouldBe` (Water (497:+13) (Stop Flood))

    let uf =  addWater (Water (497:+10) (Stop Flood))
            $ addWater (Water (497:+11) (Stop Flood)) 
            $ addWater (Water (497:+12) (Stop Flood))
            $ addWater (Water (497:+13) (Stop Flood)) ue
    it "f1" $ move uf (Water (498:+9) GoLeft) `shouldBe` (Water (497:+9) GoLeft)
    it "f2" $ move uf (Water (497:+9) GoLeft) `shouldBe` (Water (497:+9) (Stop Flood))
    it "f last" $ findLast uf `shouldBe` (Water (497:+9) (Stop Flood))

    let ug =  addWater (Water (497:+9) (Stop Flood)) uf
    it "g1" $ move ug (Water (499:+9) GoLeft) `shouldBe` (Water (498:+9) GoLeft)
    it "g2" $ move ug (Water (498:+9) GoLeft) `shouldBe` (Water (499:+9) (GoRight Flood))
    it "g3" $ move ug (Water (504:+9) (GoRight Flood)) `shouldBe` (Water (505:+9) (GoRight Flood))
    it "g4" $ move ug (Water (505:+9) (GoRight Flood)) `shouldBe` (Water (505:+10) GoDown)
    it "g5" $ move ug (Water (505:+13) GoDown) `shouldBe` (Water (505:+13) (Stop Flood))
    it "g last" $ findLast ug `shouldBe` (Water (505:+13) (Stop Flood))

    let uh =  addWater (Water (505:+10) (Stop Flood))
            $ addWater (Water (505:+11) (Stop Flood)) 
            $ addWater (Water (505:+12) (Stop Flood))
            $ addWater (Water (505:+13) (Stop Flood)) ug
    it "h1" $ move uh (Water (504:+9) (GoRight Flood)) `shouldBe` (Water (505:+9) (GoRight Flood))
    it "h2" $ move uh (Water (505:+9) (GoRight Flood)) `shouldBe` (Water (505:+9) (Stop Flood))
    it "h last" $ findLast uh `shouldBe` (Water (505:+9) (Stop Flood))

    let ui =  addWater (Water (505:+9) (Stop Flood)) uh
    it "i1" $ move ui (Water (503:+9) (GoRight Flood)) `shouldBe` (Water (504:+9) (GoRight Flood))
    it "i2" $ move ui (Water (504:+9) (GoRight Flood)) `shouldBe` (Water (504:+9) (StopAndFill Flood))
    it "i last" $ findLast ui `shouldBe` (Water (504:+9) (StopAndFill Flood))

    let uj =  fillRow (Water (504:+9) (StopAndFill Flood)) ui
    it "j1" $ move uj (Water (502:+8) GoDown) `shouldBe` (Water (502:+8) (Stop Flood))
    it "j last" $ findLast uj `shouldBe` (Water (502:+8) (Stop Flood))

    let uk =  addWater (Water (502:+2) (Stop Flood))
            $ addWater (Water (502:+3) (Stop Flood))
            $ addWater (Water (502:+4) (Stop Flood))
            $ addWater (Water (502:+5) (Stop Flood))
            $ addWater (Water (502:+6) (Stop Flood))
            $ addWater (Water (502:+7) (Stop Flood))
            $ addWater (Water (502:+8) (Stop Flood)) uj
    it "k1" $ move uk (Water (501:+2) (GoRight Clay)) `shouldBe` (Water (501:+2) (StopAndFill Flood))
    it "k last" $ findLast uk `shouldBe` (Water (501:+2) (StopAndFill Flood))

  describe "fill underground" $ do
    it "example" $ countWater (fillUnderground u) `shouldBe` 57

    it "example x" $ countWater (fillUnderground ux) `shouldBe` 102
    it "example y" $ countWater (fillUnderground uy) `shouldBe` 174
    it "example z" $ countWater (fillUnderground uz) `shouldBe` 174

    it "part 1" $ do
      input <- readFile "test/D17.txt";
      countWater (fillUnderground $ mkUnderground input) `shouldBe` 31383

    it "part 2" $ do
      input <- readFile "test/D17.txt";
      countGroundwater (fillUnderground $ mkUnderground input) `shouldBe` 25376