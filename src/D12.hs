module D12
    ( PlantRule(..), World(..), mkPlantRule, mkPlants, mkWorld, findMatchingRules, wait, eval, future
    ) where

import Data.List
import Data.Maybe

data PlantRule = Birth [Int] | Death [Int] deriving (Show, Eq)
type Plant = Int
data World = World { plants :: [Plant]
                   , rules  :: [PlantRule]
                   , generation :: Int } deriving (Show, Eq)

alive :: PlantRule -> [Int]
alive (Birth a) = a
alive (Death a) = a

mkPlantRule :: String -> Maybe PlantRule
mkPlantRule s = if void then Nothing else Just $ kind (indexes '#')
  where indexes c = filter (\i -> (s!!(i+2)) == c) [-2,-1,1,2]
        kind = if (s!!9) == '#' then Birth else Death
        void = (s!!9) == (s!!2)

mkPlants :: String -> [Plant]
mkPlants = map fst . filter ((==) '#' . snd) . zip [0..] . drop 15

mkWorld :: String -> World
mkWorld s = World (mkPlants $ head ss) (mapMaybe mkPlantRule $ drop 2 ss) 0 where ss = lines s

findMatchingRules :: World -> Int -> [(Int,PlantRule)]
findMatchingRules (World plants rules _) index = map (\x -> (index,x)) matchingRules
  where neighbours = sort $ map (flip (-) index) $ filter (\p -> p/=index && p>=(index-2) && p<=(index+2)) plants
        matchingRules = filter (\r -> (alive r)==neighbours) rules

wait :: World -> World
wait w@(World plants rules n) = World (sort $ union births $ plants \\ deaths) rules (n+1)
  where range = [(minimum plants)-2..(maximum plants)+2]
        appliedRules = concatMap (findMatchingRules w) range
        birth (i,Birth _) = Just i
        birth (i,Death _) = Nothing
        births = intersect (range \\ plants) (mapMaybe birth appliedRules)
        death (i,Birth _) = Nothing
        death (i,Death _) = Just i
        deaths = intersect plants $ mapMaybe death appliedRules

eval :: Int -> World -> Int
eval gen = sum . plants . head . drop gen . iterate wait

future :: Int -> World -> Int
future gen world = (gen - g0)*gradient + sum pl1
  where signature w = (\p -> (sum p)-(length p)*(head p)) $ plants w
        timeline = iterate wait world
        stable (w1,w2) = (signature w1) == (signature w2)
        (World pl1 _ g0,World pl2 _ _) = head $ dropWhile (not . stable) $ zip timeline $ tail timeline
        gradient = (length pl1) * ( (head pl2)-(head pl1) )
   