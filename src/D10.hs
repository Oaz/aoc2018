module D10
    ( Sky(..), Coordinates(..), mkSky, findMessage
    ) where

import Data.List
import Text.Regex

{- ########## Coordinates ########## -}
infix  6  :+
data Coordinates  = Int :+ Int deriving (Eq, Show, Read)

instance Num Coordinates where
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs (x:+y)          =  0:+0
    signum (x:+y)       =  0:+0
    fromInteger n       =  fromInteger n :+ 0

x :: Coordinates -> Int
x (a :+ b) = a

y :: Coordinates -> Int
y (a :+ b) = b

translate :: Coordinates -> [Coordinates] -> [Coordinates]
translate c = map ((+) c)

boundingBox :: [Coordinates] -> (Coordinates,Coordinates)
boundingBox pts = (minimum xs :+ minimum ys, maximum xs :+ maximum ys)
  where xs = map x pts
        ys = map y pts

{- ########## Sky ########## -}
data Sky = Sky Int [(Coordinates,Coordinates)] deriving (Eq)

instance Show Sky where 
  show (Sky a pts) = unlines $ [age] ++ [[draw (x':+y') | x'<-[0..(x size)]] |y'<-[0..(y size)]]
    where age = show a
          roughCoords = map fst pts
          (minpoint,maxpoint) = boundingBox roughCoords
          coords = translate (-minpoint) roughCoords
          size = maxpoint - minpoint
          draw p = if elem p coords then '#' else '.'

findMessage :: Int -> Sky -> Sky
findMessage threshold = head . dropWhile (not . hasMessage) . iterate wait
  where wait (Sky a s) = Sky (a+1) $ map movePoint s
        movePoint (position,direction) = (position+direction,direction)
        hasMessage (Sky _ pts) = (<threshold) $ abs $ y $ maxpoint-minpoint
          where (minpoint,maxpoint) = boundingBox $ map fst pts

mkSky :: String -> Sky
mkSky = Sky 0 . map (mkPoint . extractFromRegex "position=<(.+), (.+)> velocity=<(.+), (.+)>") . lines
  where mkPoint ps = ((ps!!0):+(ps!!1),(ps!!2):+(ps!!3))

extractFromRegex :: (Read a) => String -> String -> [a]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> map read subs) $ matchRegexAll (mkRegex regex) def
