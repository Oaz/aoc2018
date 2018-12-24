module D23
    ( Nanobot(..), Octahedron(..), Coords(..), Search(..), Win(..)
    , mkNanobots, countInRangeOfStrongest, bestCoords
    ) where

import Data.List
import Data.Ord
import Text.Regex
import Control.Exception
import Data.Maybe

{- ######### 3D coordinates ######### -}

data Coords  = Undefined | Coords { x :: Int
                                  , y :: Int
                                  , z :: Int } deriving (Eq, Show)

instance Num Coords where
    (Coords x y z) + (Coords x' y' z') = Coords (x+x') (y+y') (z+z')
    (Coords x y z) - (Coords x' y' z') = Coords (x-x') (y-y') (z-z')
    _ * _                              = Undefined
    negate (Coords x y z)              = Coords (negate x) (negate y) (negate z)
    abs (Coords x y z)                 = Undefined
    signum _                           = Undefined
    fromInteger n                      = Undefined

distance :: Coords -> Int
distance (Coords x y z) = (abs x) + (abs y) + (abs z)

hom :: Int -> Coords -> Coords
hom k (Coords x y z) = Coords (k*x) (k*y) (k*z)
          
{- ######### Octahedron ######### -}

data Octahedron = Octahedron { radius :: Int
                             , position :: Coords } deriving (Eq, Show)

intersects :: Octahedron -> Octahedron -> Bool
intersects (Octahedron r1 c1) (Octahedron r2 c2) = (distance $ (c1-c2)) <= (r1+r2)

countIntersections :: Octahedron -> [Octahedron] -> Int
countIntersections oh ohs = sum $ map fromEnum $ map (intersects oh) ohs

distanceToOrigin :: Octahedron -> Int
distanceToOrigin (Octahedron r c) = minimum $ map distance pts
  where pts = map ((+)c) [Coords x y z| x<-[-r,r], y<-[-r,r], z<-[-r,r]]

boundary :: [Octahedron] -> Octahedron
boundary ohs = Octahedron r $ Coords 0 0 0
  where m = maximum $ map (distance.position) ohs
        r = head $ filter ((<=)m) [2^n | n<-[1..]]

octos :: Octahedron -> [Octahedron]
octos (Octahedron r c) = nub $ (mkOcto r2 base1) ++ (mkOcto (r2+r4) base2)
  where r2 = div r 2
        r4 = div r 4
        base1 = map (hom r2) [ Coords 1 0 0, Coords (-1) 0 0, Coords 0 1 0, Coords 0 (-1) 0
                , Coords 0 0 1, Coords 0 0 (-1) ]
        base2 = map (hom r4) [ Coords x y z | x<-[-1,1], y<-[-1,1], z<-[-1,1] ]
        mkOcto r' us = map ((Octahedron r').((+)c)) us

{- ######### Nanobot ######### -}

type Nanobot = Octahedron

extractFromRegex :: (Read a) => String -> String -> [a]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> map read subs) $ matchRegexAll (mkRegex regex) def
      
mkNanobots :: String -> [Nanobot]
mkNanobots = map (mkNanobot . extractFromRegex "pos=<([-0-9]+),([-0-9]+),([-0-9]+)>, r=([-0-9]+)") . lines
  where mkNanobot (x:y:z:r:[]) = Octahedron r $ Coords x y z

{- ######### part 1 ######### -}

countInRangeOfStrongest :: [Nanobot] -> Int
countInRangeOfStrongest ns = countInRange $ strongest ns
  where strongest = maximumBy (comparing radius)
        countInRange n = countIntersections n $ map (Octahedron 0 . position) ns

{- ######### part 2 ######### -}

data Win = Win { coords :: Coords
               , score :: Int
               , dto :: Int } deriving (Eq, Show)

faceToFace :: Win -> Win -> Win
faceToFace w1@(Win _ s1 d1) w2@(Win _ s2 d2)
  | d1 < d2   = assert (s1 == s2) w1
  | otherwise = assert (s1 == s2) w2

data Candidate = CD { geo :: Octahedron
                    , botCount :: Int } deriving (Eq, Show)
instance Ord Candidate where
  compare (CD _ s1) (CD _ s2)
    | s1 > s2   = LT
    | s1 < s2   = GT
    | otherwise = EQ

data Search = Search { candidates :: [Candidate]
                     , winner :: Maybe Win } deriving (Eq, Show)

mkCandidates :: [Nanobot] -> Octahedron -> [Candidate]
mkCandidates ns = map computeScore . octos
  where computeScore oh = CD oh $ countIntersections oh ns

searchPhase :: [Nanobot] -> Search -> Search
searchPhase ns (Search ((CD (Octahedron 0 c) score):oss) _) = Search (nub $ removeLosers score oss)
                                                                $ Just $ Win c score $ distance c
searchPhase ns (Search (CD oh s                :oss) _) = Search newCandidates Nothing
  where newCandidates = removeBadCandidates $ flip (++) oss $ mkCandidates ns oh

removeBadCandidates :: [Candidate] -> [Candidate]
removeBadCandidates oss = filterByWinscoreLowerBound $ find (((==)0).radius.geo) sorted
  where filterByWinscoreLowerBound Nothing = sorted
        filterByWinscoreLowerBound (Just refCD) = filter ((<=)refCD) sorted
        sorted = sort oss

winnersPhase :: [Nanobot] -> Search -> Search
winnersPhase ns (Search ((CD (Octahedron 0 c) score):oss) (Just currentWinner)) =
                                    Search oss $ Just $ faceToFace currentWinner $ Win c score $ distance c
winnersPhase ns (Search (CD oh s:oss) winner) = Search (nub $ addCandidates++oss) winner
  where addCandidates = reduceScope winner $ mkCandidates ns oh
        reduceScope (Just (Win _ wscore wdist)) = filter (canWin wdist) . removeLosers wscore
        canWin r (CD oh _) = r < distanceToOrigin oh

removeLosers :: Int -> [Candidate] -> [Candidate]
removeLosers winscore oss = filter (((==)winscore).botCount) oss

bestCoords :: [Nanobot] -> Win
bestCoords ns = fromJust $ winner finalResult
  where startingPool = Search [CD (boundary ns) 0] Nothing
        after1stWin = loopWhile startingPool (searchPhase ns) (isNothing.winner)
        finalResult = loopWhile after1stWin (winnersPhase ns) (((/=)0).length.candidates)
        loopWhile init loop cond = head $ dropWhile cond $ iterate loop init
