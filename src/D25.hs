module D25
    ( Point(..), mkPoints, mkGalaxy, mkConstellation, mkConstellations, countConstellations
    ) where

import Data.List
import Text.Regex
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as I
import qualified Data.Sequence as S
import Data.Foldable
import Data.Functor

{- ######### points ######### -}

data Point = Point  { x :: Int
                    , y :: Int
                    , z :: Int
                    , t :: Int } deriving (Eq, Show)

extractFromRegex :: (Read a) => String -> String -> [a]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> map read subs) $ matchRegexAll (mkRegex regex) def
      
mkPoints :: String -> [Point]
mkPoints = map (mkPoint . extractFromRegex "([-0-9]+),([-0-9]+),([-0-9]+),([-0-9]+)") . lines
  where mkPoint (x:y:z:t:[]) = Point x y z t

distance :: Point -> Point -> Int
distance (Point x y z t) (Point x' y' z' t') = (abs $ x-x') + (abs $ y-y') + (abs $ z-z') + (abs $ t-t')

{- ######### galaxy ######### -}
                        
type Galaxy = I.IntMap (V.Vector Int) -- for each point, a vector of distances to all other points

mkGalaxy :: [Point] -> Galaxy
mkGalaxy = I.fromList . zip [0..] . toList . computeDistances S.empty
  where computeDistances ds [] = ds
        computeDistances ds (p:pts) = computeDistances (ds S.|> distanceToOthers p pts ds) pts
        distanceToOthers p pts ds = V.fromList $ toList $ (computedBefore ds) S.>< (distancesToRemaining p pts)
        computedBefore ds = fmap (flip (V.!) $ length ds) ds
        distancesToRemaining p pts = 0 S.<| S.fromList (map (distance p) pts)
                
{- ######### constellations ######### -}

type Constellation = V.Vector Int

mkConstellation :: Galaxy -> (Constellation,Galaxy)
mkConstellation g = growConstellation (g I.! startIdx) (I.delete startIdx g)
  where startIdx = head $ I.keys g

growConstellation :: Constellation -> Galaxy -> (Constellation,Galaxy)
growConstellation c g = go $ V.findIndex shouldMerge c
  where shouldMerge d = d<=3 && d>0
        go Nothing    = (c,g)
        go (Just idx) = growConstellation (V.zipWith min c (g I.! idx)) (I.delete idx g)

mkConstellations :: Galaxy -> [Constellation]
mkConstellations = go []
  where go cs g = let (c,g') = mkConstellation g
                  in if I.null g then cs else go (c:cs) g'

countConstellations :: [Point] -> Int
countConstellations = length . mkConstellations . mkGalaxy
