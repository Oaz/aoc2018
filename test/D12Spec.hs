module D12Spec (spec) where

import Test.Hspec
import D12
import Data.Maybe

spec :: Spec
spec = do

  describe "parse rules" $ do
    it "birth"  $ mkPlantRule "...## => #" `shouldBe` Just (Birth [1,2])
    it "other birth"  $ mkPlantRule "##.#. => #" `shouldBe` Just (Birth [-2,-1,1])
    it "death"  $ mkPlantRule ".##.. => ." `shouldBe` Just (Death [-1])
    it "remain alive"  $ mkPlantRule ".##.. => #" `shouldBe` Nothing
    it "remain dead"  $ mkPlantRule ".#... => ." `shouldBe` Nothing

  describe "parse plants" $ do
    it "some plants"  $ mkPlants "initial state: #..#.#..##..#.." `shouldBe` [0,3,5,8,9,12]
    it "other plants"  $ mkPlants "initial state: #.#..#......#..#" `shouldBe` [0,2,5,12,15]

  describe "parse world" $ do
    it "one world"  $ mkWorld "initial state: #..#.#\n\n...## => #\n.##.. => .\n" `shouldBe` World [0,3,5] [Birth [1,2],Death [-1]] 0

  let birth = Birth [-1,1]
  let death = Death [-2,-1,1,2]
  let rules = [birth,death]
  let rules0 = [ Birth [1],Death [],Birth [-1,2],Death [1],Death [-1],Birth [-2,-1,2],Birth [-2,1],Death [-2,-1,1],Birth [-1] ]           

  describe "find rules" $ do
    it "match nothing"  $ findMatchingRules (World [0,2,4] rules 0) 0 `shouldBe` []
    it "match other nothing"  $ findMatchingRules (World [0,2,4] rules 0) 2 `shouldBe` []
    it "match birth"  $ findMatchingRules (World [0,2,4] rules 0) 1 `shouldBe` [(1,birth)]
    it "match other birth"  $ findMatchingRules (World [0,2,4] rules 0) 3 `shouldBe` [(3,birth)]
    it "match outside"  $ findMatchingRules (World [0,2,4] rules 0) (-2) `shouldBe` []
    it "match death"  $ findMatchingRules (World [0,1,2,3,4] rules 0) 2 `shouldBe` [(2,death)]
    it "match death again"  $ findMatchingRules (World [2,3] [Death [1]] 0) 2 `shouldBe` [(2,Death [1])]
    it "match birth and death"  $ findMatchingRules (World [1,4,5,8] rules0 0) 4 `shouldBe` [(4,Birth [1]), (4,Death [1])]

  let rules2 = [Birth [-2,-1], Death [1,2]]
  let rules3 = [Death [-2,-1], Birth [1,2]]

  describe "watch world" $ do
    it "wait"  $ wait (World [0,2,4] rules 0) `shouldBe` (World [0,1,2,3,4] rules 1)
    it "wait again"  $ wait (World [0,1,2,3,4] rules 0) `shouldBe` (World [0,1,3,4] rules 1)
    it "wait still"  $ wait (World [0,1,3,4] rules 0) `shouldBe` (World [0,1,3,4] rules 1)
    it "wait2a"  $ wait (World [0,1] rules2 0) `shouldBe` (World [0,1,2] rules2 1)
    it "wait2b"  $ wait (World [0,1,2] rules2 0) `shouldBe` (World [1,2,3] rules2 1)
    it "wait2c"  $ wait (World [1,2,3] rules2 0) `shouldBe` (World [2,3,4] rules2 1)
    it "wait3a"  $ wait (World [0,1] rules3 0) `shouldBe` (World [-1,0,1] rules3 1)
    it "wait3b"  $ wait (World [-1,0,1] rules3 0) `shouldBe` (World [-2,-1,0] rules3 1)
    it "wait3c"  $ wait (World [-2,-1,0] rules3 0) `shouldBe` (World [-3,-2,-1] rules3 1)
    it "wait4a"  $ wait (World [2,3] [Death [1],Death [-1]] 0) `shouldBe` (World [] [Death [1],Death [-1]] 1)
    it "wait0a"  $ wait (World [1,4,5,8] rules0 0) `shouldBe` (World [0,2,6,7,9] rules0 1)
        
  describe "eval world" $ do
    it "eval"  $ eval 20 (World [0,2,4] rules 0) `shouldBe` 8
    it "eval 2"  $ eval 3 (World [0,1] rules2 0) `shouldBe` 9
    it "eval 3"  $ eval 3 (World [0,1] rules3 0) `shouldBe` (-6)
    it "submits answer" $ do input <- readFile "test/D12.txt"; (eval 20 $ mkWorld input) `shouldBe` 3605
       
  describe "future" $ do
    it "submits answer" $ do input <- readFile "test/D12.txt"; (future 50000000000 $ mkWorld input) `shouldBe` 4050000000798
