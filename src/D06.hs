module D06
    ( Coordinates(..), x, y, manhattan, quadrant, core, translate, area, maximalArea, mkCoordinates, distanceBelow, maximalArea2
    ) where

import Data.List
import Data.Function
import Data.Ord
import Data.Maybe
import Control.Applicative
import Text.Regex

infix  6  :+
data Coordinates  = Int :+ Int deriving (Eq, Show, Read)

instance Num Coordinates where
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs (x:+y)          =  ((abs x) + (abs y)) :+ 0
    signum (x:+y)
        | (abs x) > (abs y) = (signum x) :+ 0
        | (abs x) < (abs y) = 0 :+ (signum y)
        | otherwise = (signum x) :+ (signum y)
    fromInteger n       =  fromInteger n :+ 0

x :: Coordinates -> Int
x (a :+ b) = a

y :: Coordinates -> Int
y (a :+ b) = b

manhattan :: Coordinates -> Int
manhattan c = x $ abs c

quadrant :: Coordinates -> [Bool]
quadrant c = [1 == x sn, 1 == y sn, -1 == x sn, -1 == y sn] where sn = signum c

core :: [Coordinates] -> [Coordinates]
core = nub . concat . (<*>) (map coreQuadrantFilter [0..3]) . (:[])
  where
    coreQuadrantFilter = (\q -> coreQuadrant . quadrantFilter q) . flip (!!)
    quadrantFilter q = filter (q . quadrant)
    coreQuadrant [] = []
    coreQuadrant xs = (:[]) $ minimumBy (comparing manhattan) xs

translate :: Coordinates -> [Coordinates] -> [Coordinates]
translate c = map ((+) c)
    
fill :: [Coordinates] -> [Coordinates]
fill cs = [a:+b | a<-[(minimum xs)..(maximum xs)], b<-[(minimum ys)..(maximum ys)]]
    where
      xs = map x cs
      ys = map y cs

area :: [Coordinates] -> Coordinates -> Maybe Int
area cs c
  | elem c grid = Just $ length $ filter ((==) c) $ mapMaybe (closest cs) grid
  | otherwise = Nothing
  where grid = fill $ translate c $ core $ translate (-c) cs

closest :: [Coordinates] -> Coordinates -> Maybe Coordinates
closest cs o = if length closests == 1 then Just $ fst $ head $ closests else Nothing
    where
      closests = filter (\(c,d) -> d == shortestDistance) withDistances
      shortestDistance = minimum $ map snd withDistances
      withDistances = map (\c -> (c,manhattan (c-o))) cs

maximalArea :: [Coordinates] -> Maybe Int
maximalArea cs
  | areas == [] = Nothing
  | otherwise = Just $ maximum areas
  where areas = mapMaybe (area cs) cs

extractFromRegex :: (Read a) => String -> String -> [a]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> map read subs) $ matchRegexAll (mkRegex regex) def
      
mkCoordinates :: String -> [Coordinates]
mkCoordinates = map ((\p -> (head p) :+ (last p)) . extractFromRegex "([0-9]+), ([0-9]+)") . lines

distanceBelow :: Int -> [Coordinates] -> Coordinates -> Maybe Int
distanceBelow threshold cs origin = foldl sumUntilThreshold (Just 0) (translate (-origin) cs)
  where
    sumUntilThreshold Nothing _ = Nothing
    sumUntilThreshold (Just distance) c = if newDistance >= threshold then Nothing else (Just newDistance)
      where
        newDistance = distance + manhattan c
        
maximalArea2 :: Int -> [Coordinates] -> Int
maximalArea2 threshold cs = length $ mapMaybe (distanceBelow threshold cs) $ fill cs
