module D09
    ( MarbleCircle(..), marbleRounds, highestScore
    ) where

import Data.List
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M

type MarbleRules = (Int,Int)
data MarbleCircle = MarbleCircle { marbleRound :: Int,
                                   current :: Int,
                                   marbles :: S.Seq Int,
                                   score :: Int } deriving (Show, Eq)

marbleRounds :: MarbleRules -> [MarbleCircle]
marbleRounds rules = iterate (nextRound rules) initialCircle

initialCircle :: MarbleCircle
initialCircle = MarbleCircle 0 0 (S.singleton 0) 0

nextRound :: MarbleRules -> MarbleCircle -> MarbleCircle
nextRound rules@(period,shift) circle
  | mod newRound period > 0 = regularRound newRound circle
  | otherwise               = scoringRound rules newRound circle
  where newRound = (marbleRound circle) + 1

regularRound :: Int -> MarbleCircle -> MarbleCircle
regularRound newRound (MarbleCircle _ cIndex cMarbles _) = MarbleCircle newRound newIndex (S.insertAt newIndex newRound cMarbles) 0
  where (_,newIndex) = gotoNewIndex (cIndex+2) cMarbles 

scoringRound :: MarbleRules -> Int -> MarbleCircle -> MarbleCircle
scoringRound (period,shift) newRound (MarbleCircle _ cIndex cMarbles _) = MarbleCircle newRound newIndex newMarbles score
  where (splitter,newIndex) = gotoNewIndex (cIndex-shift) cMarbles
        score = newRound+(S.index cMarbles splitter)
        newMarbles = S.deleteAt splitter cMarbles

gotoNewIndex :: Int -> S.Seq Int -> (Int,Int)
gotoNewIndex index marbles = (moveIndex, if moveIndex > 0 then moveIndex else size)
  where size = S.length marbles
        moveIndex = mod index size

playerScores :: MarbleRules -> Int -> Int -> M.Map Int Int
playerScores rules@(period,_) nbPlayers lastMarble = M.fromListWith (+) $ take (div lastMarble period) $ zip players scores
  where players = map (flip mod nbPlayers) $ iterate (+period) 0
        scores = filter (/=0) $ map score $ marbleRounds rules

highestScore :: Int -> Int -> Int
highestScore nbPlayers lastMarble = maximum $ M.elems $ playerScores (23,7) nbPlayers lastMarble
