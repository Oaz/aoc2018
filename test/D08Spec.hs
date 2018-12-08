module D08Spec (spec) where

import Test.Hspec
import D08
import Data.Tree

spec :: Spec
spec = do
    let steps = [ Cursor []                                                                       [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]
                , Cursor [Block 2 3 []]                                                           [0,3,10,11,12,1,1,0,1,99,2,1,1,2]
                , Cursor [Block 1 3 [Node [10,11,12] []]]                                         [1,1,0,1,99,2,1,1,2]
                , Cursor [Block 1 1 [], Block 0 3 [Node [10,11,12] []]]                           [0,1,99,2,1,1,2]
                , Cursor [Block 0 1 [Node [99] []], Block 0 3 [Node [10,11,12] []]]               [2,1,1,2]
                , Cursor [Block 0 3 [Node [2] [Node [99] []], Node [10,11,12] []]]                [1,1,2]
                , Cursor [Block 0 0 [Node [1,1,2] [Node [10,11,12] [], Node [2] [Node [99] []]]]] [] ]

    let sampleTree = parseLicense "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

    describe "build tree" $ do
      it "open A root"  $ moveCursor (steps !! 0) `shouldBe` steps !! 1
      it "add B leaf"   $ moveCursor (steps !! 1) `shouldBe` steps !! 2
      it "open C node"  $ moveCursor (steps !! 2) `shouldBe` steps !! 3
      it "add D leaf"   $ moveCursor (steps !! 3) `shouldBe` steps !! 4 
      it "close C node" $ moveCursor (steps !! 4) `shouldBe` steps !! 5
      it "close A root" $ moveCursor (steps !! 5) `shouldBe` steps !! 6
    
    describe "metadata size" $ do
      it "sample stream"  $ metadataSum sampleTree `shouldBe` 138
      it "submits answer" $ do input <- readFile "test/D08.txt"; (metadataSum $ parseLicense input) `shouldBe` 40746

    describe "metadata size by index" $ do
      it "size by index of B node (sum)"  $ metadataSumByIndex ((!!0) $ subForest sampleTree) `shouldBe` 33
      it "size by index of D node (sum)"  $ metadataSumByIndex ((!!0) $ subForest $ (!!1) $ subForest sampleTree) `shouldBe` 99
      it "size by index of C node"  $ metadataSumByIndex ((!!1) $ subForest sampleTree) `shouldBe` 0
      it "size by index of A root node"  $ metadataSumByIndex sampleTree `shouldBe` 66
      it "submits answer" $ do input <- readFile "test/D08.txt"; (metadataSumByIndex $ parseLicense input) `shouldBe` 37453
