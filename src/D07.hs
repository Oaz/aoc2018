module D07
    ( Step(..), stepSort, findNextChars, Work(..), mkWork, assignWorker, proceed, compute, parseRules
    ) where

import Data.List
import qualified Data.PartialOrd as PO
import Text.Regex

type Rules = [(Char,Char)]
data Step = Step Rules Char deriving (Eq, Show, Read)

instance PO.PartialOrd Step where
    s <= s' = (isRule s s') || (or $ map (PO.<= s') $ map (Step $ rules s) $ map snd $ rulesStartingWith s)
      where
        isRule (Step r c) (Step _ c') = elem (c,c') r
        rules (Step r _) = r
        rulesStartingWith (Step r c) = filter ((==c).fst) r

names :: Rules -> String
names rules = sort $ nub $ union (map fst rules) (map snd rules)

chars :: [Step] -> String
chars = map (\(Step _ c) -> c)

findNextChars :: Rules -> String -> String
findNextChars rules candidates = (sort . chars . PO.minima) $ (map . Step) rules candidates

stepSort :: Rules -> String
stepSort rules = snd $ head $ dropWhile ((>0).length.fst) $ iterate findNext $ flip (,) [] $ names rules
  where findNext (remainingChars,sortedChars) = (\nextChar -> (remainingChars \\ nextChar, sortedChars ++ nextChar)) $ (:[]) $ head $ findNextChars rules remainingChars

data Work = Work { baseCost :: Int,
                   availableWorkers :: Int,
                   ongoing :: [(Char,Int)],
                   remaining :: [Char],
                   timeSpent :: Int,
                   rules :: Rules }
  deriving (Show, Eq)

mkWork :: Int -> Int -> Rules -> Work
mkWork baseCost availableWorkers rules = Work baseCost availableWorkers [] (names rules) 0 rules

assignWorker :: Work -> Work
assignWorker w @ Work { availableWorkers = 0 } = w
assignWorker w = w { availableWorkers = (availableWorkers w) - (length stepsToStart), ongoing = (ongoing w) ++ (map addCost stepsToStart), remaining = (remaining w) \\ stepsToStart } 
  where
    ongoingTasks = map fst $ ongoing w
    stepCandidates = (findNextChars (rules w) (ongoingTasks ++ (remaining w))) \\ ongoingTasks
    stepsToStart = take (availableWorkers w) stepCandidates
    addCost c = (c, (baseCost w) + 1 + fromEnum c - fromEnum 'A')

proceed :: Work -> Work
proceed w = w { availableWorkers = (availableWorkers w) + newWorkers, ongoing = after, timeSpent = (timeSpent w) + 1 }
  where
    newWorkers = (length $ ongoing w) - (length after)
    after = concatMap executeOngoing $ map (:[]) (ongoing w)
    executeOngoing [(c,remaining)] = if remaining == 1 then [] else [(c,remaining-1)]

compute :: Work -> Int
compute = timeSpent . head . dropWhile hasThingsToDo . iterate (proceed . assignWorker)
  where hasThingsToDo w = ((length $ ongoing w) > 0) || ((length $ remaining w) > 0)

parseRules :: String -> Rules
parseRules = map (\p -> (p!!5,p!!36)) . lines
