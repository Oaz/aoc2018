module D07Spec (spec) where

import Test.Hspec
import D07
import qualified Data.PartialOrd as PO

spec :: Spec
spec = do
    let rules = [('C','A'),('D','E'),('C','F'),('A','D'),('B','E'),('A','B'),('F','E')]

    describe "sort" $ do
      it "A C" $ (Step rules 'A') PO.<= (Step rules 'C') `shouldBe` False
      it "C A" $ (Step rules 'C') PO.<= (Step rules 'A') `shouldBe` True
      it "F B" $ (Step rules 'F') PO.<= (Step rules 'B') `shouldBe` False
      it "C E" $ (Step rules 'C') PO.<= (Step rules 'E') `shouldBe` True
      it "findNext" $ findNextChars rules "ABCDEF" `shouldBe` "C"
      it "findNext again" $ findNextChars rules "FDBAE" `shouldBe` "AF"
      it "findNext and again" $ findNextChars rules "FDBE" `shouldBe` "BDF"
      it "sort" $ stepSort rules `shouldBe` "CABDFE"
      it "submits answer" $ do input <- readFile "test/D07.txt"; (stepSort $ parseRules input) `shouldBe` "BCADPVTJFZNRWXHEKSQLUYGMIO"

    describe "workers" $ do
      let sampleWork = mkWork 0 2 rules
      it "assign worker" $ assignWorker sampleWork `shouldBe` Work 0 1 [('C',3)] "ABDEF" 0 rules
      it "proceed" $ proceed (Work 0 1 [('C',3)] "ABDEF" 0 rules) `shouldBe` Work 0 1 [('C',2)] "ABDEF" 1 rules
      it "do not assign worker with ongoing dependency" $ assignWorker (Work 0 1 [('C',3)] "ABDEF" 0 rules) `shouldBe` Work 0 1 [('C',3)] "ABDEF" 0 rules
      it "proceed and release" $ proceed (Work 0 1 [('C',1)] "ABDEF" 2 rules) `shouldBe` Work 0 2 [] "ABDEF" 3 rules
      it "assign multiple workers" $ assignWorker (Work 0 2 [] "ABDEF" 3 rules) `shouldBe` Work 0 0 [('A',1),('F',6)] "BDE" 3 rules 
      it "proceed multiple tasks" $ proceed (Work 0 0 [('A',1),('F',6)] "BDE" 3 rules) `shouldBe` Work 0 1 [('F',5)] "BDE" 4 rules
      it "compute full work" $ compute sampleWork `shouldBe` 15
      it "submits answer" $ do input <- readFile "test/D07.txt"; (compute $ mkWork 60 5 $ parseRules input) `shouldBe` 973

 