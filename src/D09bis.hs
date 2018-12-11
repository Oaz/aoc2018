module D09bis
    ( RotatingBezel(..), rotate, binsert, bdelete, bfromList, MarbleCircle(..), marbleRounds, highestScore
    ) where

import Data.List
import qualified Data.Map.Strict as M

{- ########### Rotating Bezel ########### -}
data RotatingBezel a = RotatingBezel [a] a [a] deriving (Show)

instance (Eq a) => Eq (RotatingBezel a) where
  x == y = cut x == cut y

cut :: RotatingBezel a -> [a]
cut (RotatingBezel l c r) = (c:r) ++ reverse l

current :: RotatingBezel a -> a
current (RotatingBezel _ c _) = c

bsingleton :: a -> RotatingBezel a
bsingleton x =  RotatingBezel [] x []

bfromList :: [a] -> RotatingBezel a
bfromList (x:xs) =  RotatingBezel [] x xs
    
clockwise :: RotatingBezel a -> RotatingBezel a
clockwise b@(RotatingBezel [] _ []) = b
clockwise (RotatingBezel ls c []) = RotatingBezel [c] x xs where (x:xs) = reverse ls
clockwise (RotatingBezel ls c (r:rs)) = RotatingBezel (c:ls) r rs

cclockwise :: RotatingBezel a -> RotatingBezel a
cclockwise b@(RotatingBezel [] _ []) = b
cclockwise (RotatingBezel [] c rs) = RotatingBezel xs x [c] where (x:xs) = reverse rs
cclockwise (RotatingBezel (l:ls) c rs) = RotatingBezel ls l (c:rs)

rotate :: Int -> RotatingBezel a -> RotatingBezel a
rotate n
  | n > 0 = (flip (!!)) n . iterate clockwise
  | n < 0 = (flip (!!)) (-n) . iterate cclockwise
  | otherwise = id

binsert :: a -> RotatingBezel a -> RotatingBezel a
binsert d (RotatingBezel ls c rs) = (RotatingBezel (c:ls) d rs)

bdelete :: RotatingBezel a -> RotatingBezel a
bdelete (RotatingBezel ls c []) = (RotatingBezel [] x xs) where (x:xs) = reverse ls
bdelete (RotatingBezel ls c (r:rs)) = (RotatingBezel ls r rs)

{- ########### MarbleCircle ########### -}
type MarbleRules = (Int,Int)
data MarbleCircle = MarbleCircle { marbleRound :: Int,
                                   marbles :: RotatingBezel Int,
                                   score :: Int } deriving (Show, Eq)

marbleRounds :: MarbleRules -> [MarbleCircle]
marbleRounds rules = iterate (nextRound rules) initialCircle

initialCircle :: MarbleCircle
initialCircle = MarbleCircle 0 (bsingleton 0) 0

nextRound :: MarbleRules -> MarbleCircle -> MarbleCircle
nextRound rules@(period,shift) circle
  | mod newRound period > 0 = regularRound newRound circle
  | otherwise               = scoringRound rules newRound circle
  where newRound = (marbleRound circle) + 1

regularRound :: Int -> MarbleCircle -> MarbleCircle
regularRound newRound (MarbleCircle _ cMarbles _) = MarbleCircle newRound (binsert newRound $ clockwise cMarbles) 0

scoringRound :: MarbleRules -> Int -> MarbleCircle -> MarbleCircle
scoringRound (period,shift) newRound (MarbleCircle _ cMarbles _) = MarbleCircle newRound (bdelete shiftedMarbles) score
  where score = newRound + (current shiftedMarbles)
        shiftedMarbles = rotate (-shift) cMarbles

playerScores :: MarbleRules -> Int -> Int -> M.Map Int Int
playerScores rules@(period,_) nbPlayers lastMarble = M.fromListWith (+) $ take (div lastMarble period) $ zip players scores
  where players = map (flip mod nbPlayers) $ iterate (+period) 0
        scores = filter (/=0) $ map score $ marbleRounds rules

highestScore :: Int -> Int -> Int
highestScore nbPlayers lastMarble = maximum $ M.elems $ playerScores (23,7) nbPlayers lastMarble
