module D25Spec (spec) where

import Test.Hspec
import D25
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as I

spec :: Spec
spec = do

    let samplePoints1 = [ Point  0 0 0 0
                        , Point  3 0 0 0
                        , Point  0 3 0 0
                        , Point  0 0 3 0
                        , Point  0 0 0 3
                        , Point  0 0 0 6
                        , Point  9 0 0 0
                        , Point 12 0 0 0 ]

    let sampleGalaxy1 = mkGalaxy samplePoints1

    describe "points" $ do
      it "read from string" $ mkPoints  "0,0,0,0\n\
                                        \3,0,0,0\n\
                                        \0,3,0,0\n\
                                        \0,0,3,0\n\
                                        \0,0,0,3\n\
                                        \0,0,0,6\n\
                                        \9,0,0,0\n\
                                        \12,0,0,0\n" `shouldBe` samplePoints1

      it "galaxy" $ sampleGalaxy1 `shouldBe` I.fromList [ (0, V.fromList [ 0, 3, 3, 3, 3, 6, 9,12])
                                                        , (1, V.fromList [ 3, 0, 6, 6, 6, 9, 6, 9])
                                                        , (2, V.fromList [ 3, 6, 0, 6, 6, 9,12,15])
                                                        , (3, V.fromList [ 3, 6, 6, 0, 6, 9,12,15])
                                                        , (4, V.fromList [ 3, 6, 6, 6, 0, 3,12,15])
                                                        , (5, V.fromList [ 6, 9, 9, 9, 3, 0,15,18])
                                                        , (6, V.fromList [ 9, 6,12,12,12,15, 0, 3])
                                                        , (7, V.fromList [12, 9,15,15,15,18, 3, 0]) ]
    
    describe "constellations" $ do

      it "create one constellation" $
        mkConstellation sampleGalaxy1 `shouldBe` ( V.fromList [ 0, 0, 0, 0, 0, 0, 6, 9]
                                                 , I.fromList [ (6, V.fromList [ 9, 6,12,12,12,15, 0, 3])
                                                              , (7, V.fromList [12, 9,15,15,15,18, 3, 0])
                                                              ]
                                                 )

      it "create all constellations" $
        mkConstellations sampleGalaxy1 `shouldBe` [ V.fromList [ 9, 6,12,12,12,15, 0, 0]
                                                  , V.fromList [ 0, 0, 0, 0, 0, 0, 6, 9]
                                                  ]

      it "count constellations sample 2" $
        (countConstellations $ mkPoints "-1,2,2,0\n\
                                        \0,0,2,-2\n\
                                        \0,0,0,-2\n\
                                        \-1,2,0,0\n\
                                        \-2,-2,-2,2\n\
                                        \3,0,2,-1\n\
                                        \-1,3,2,2\n\
                                        \-1,0,-1,0\n\
                                        \0,2,1,-2\n\
                                        \3,0,0,0") `shouldBe` 4

      it "count constellations sample 3" $
        (countConstellations $ mkPoints "1,-1,0,1\n\
                                        \2,0,-1,0\n\
                                        \3,2,-1,0\n\
                                        \0,0,3,1\n\
                                        \0,0,-1,-1\n\
                                        \2,3,-2,0\n\
                                        \-2,2,0,0\n\
                                        \2,-2,0,-1\n\
                                        \1,-1,0,-1\n\
                                        \3,2,0,2") `shouldBe` 3

      it "count constellations sample 4" $
        (countConstellations $ mkPoints "1,-1,-1,-2\n\
                                        \-2,-2,0,1\n\
                                        \0,2,1,3\n\
                                        \-2,3,-2,1\n\
                                        \0,2,3,-2\n\
                                        \-1,-1,1,-2\n\
                                        \0,-2,-1,0\n\
                                        \-2,2,3,-1\n\
                                        \1,2,2,0\n\
                                        \-1,-2,0,-2") `shouldBe` 8

      it "submit answer"  $ do { input <- readFile "test/D25.txt"
                               ; (countConstellations $ mkPoints input) `shouldBe` 324
                               }
