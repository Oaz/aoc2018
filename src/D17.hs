module D17
    ( Coordinates(..), Geology(..), Direction(..), Underground(..), Water(..)
    , readClays, groupClays, mkUnderground, geologyAt
    , move, addWater, fillRow, findLast, fillUnderground, countWater
    , showUnderground, countGroundwater
    ) where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Ord
import Text.Regex
  
{- ########### Going through geological areas ########### -}
data Geology = Sand | Clay | Groundwater | Flood | Spring deriving (Show, Eq)
data Neighbors = Neighbors  { left :: Geology
                            , down :: Geology
                            , right :: Geology }  deriving (Show, Eq)
data Direction = GoDown | GoLeft | GoRight Geology | Stop Geology | StopAndFill Geology deriving (Show, Eq)

nextDirection :: Neighbors -> Direction -> Direction
-- when there's nothing below us...
nextDirection (Neighbors _ Sand _) _ = GoDown -- Falling
nextDirection (Neighbors _ Flood _) _ = Stop Flood -- Caught in the flood
-- otherwise
-- going down
nextDirection (Neighbors Sand _ _) GoDown = GoLeft -- Try left 1st if down is blocked
nextDirection (Neighbors g _ Sand) GoDown = GoRight g -- falling from right cliff
-- going left
nextDirection (Neighbors Sand _ _) GoLeft = GoLeft -- farther to the left
nextDirection (Neighbors g _ _) GoLeft = GoRight g -- If left blocked, try right and memorize what's on the left
-- going right
nextDirection (Neighbors _ _ Sand) (GoRight g) = GoRight g -- farther to the right
nextDirection (Neighbors _ _ Flood) (GoRight _) = StopAndFill Flood -- Caught in the flood if any on the right
nextDirection (Neighbors _ _ _) (GoRight Flood) = StopAndFill Flood --                         or on the left
-- finally
nextDirection (Neighbors _ _ _) _ = StopAndFill Groundwater -- If blocked for any other reason, become groundwater

isMoving :: Direction -> Bool
isMoving (Stop _) = False
isMoving (StopAndFill _) = False
isMoving _ = True

isWater :: Geology -> Bool
isWater Groundwater = True
isWater Flood = True
isWater _ = False

{- ########### Coordinates ########### -}
infix  6  :+
data Coordinates  = Int :+ Int deriving (Eq, Show)
instance Hashable Coordinates where
  hashWithSalt n (x:+y) = hashWithSalt n (x,y)
instance Ord Coordinates where
  compare (x:+y) (x':+y') = compare (y,x) (y',x')
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

applyDirection :: Coordinates -> Direction -> Coordinates
applyDirection (x:+y) GoDown = x:+(y+1)
applyDirection (x:+y) GoLeft = (x-1):+y
applyDirection (x:+y) (GoRight _) = (x+1):+y
applyDirection (x:+y) (Stop _) = x:+y
applyDirection (x:+y) (StopAndFill _) = x:+y

{- ########### Underground ########### -}
data Underground = Underground  { spring :: Coordinates
                                , bottom :: Int
                                , boundary :: (Coordinates, Coordinates)
                                , geology :: M.HashMap Coordinates Geology } deriving (Show, Eq)

geologyAt :: Underground -> Coordinates -> Geology
geologyAt (Underground s b _ g) c@(x:+y)
  | s == c    = Spring
  | y > b     = Flood
  | otherwise = M.lookupDefault Sand c g

isInBoundary :: Underground -> Coordinates -> Bool
isInBoundary (Underground _ _ (lbound,ubound) _) p = (p >= lbound) && (p <= (ubound+(1:+1)))  

{- ########### Moving in the underground ########### -}

data Water = Water  { position :: Coordinates
                    , direction :: Direction } deriving (Show, Eq)

move :: Underground -> Water -> Water
move u (Water pos dir) = Water (applyDirection pos newDirection) newDirection
  where neighborPositions = map ((+) pos) [(-1):+0, 0:+1, 1:+0]
        (l:d:r:[]) = map (geologyAt u) neighborPositions
        newDirection = nextDirection (Neighbors l d r) dir

addWater :: Water -> Underground -> Underground
addWater (Water pos (Stop x)) u@(Underground _ _ _ g) = u { geology = M.insert pos x g }

fillRow :: Water -> Underground -> Underground
fillRow (Water pos@(_:+y) (StopAndFill v)) u@(Underground _ _ _ g) = u { geology = foldr addGeology g toFill }
  where findLimit d = head $ dropWhile (((==)Sand).(geologyAt u)) $ iterate ((+)d) pos :: Coordinates
        (l:+_,r:+_) = (findLimit $ (-1):+0, findLimit $ 1:+0)
        toFill = [x:+y | x<-[(l+1)..(r-1)]]
        addGeology p = M.insert p v
                
findLast :: Underground -> Water
findLast u@(Underground startP _ _ _) = head $ dropWhile (isMoving.direction) $ iterate (move u) (Water startP GoDown)
                            
processWater :: Underground -> Water -> Underground
processWater u w@(Water _ (StopAndFill _)) = fillRow w u
processWater u w@(Water _ (Stop _)) = addWater w u

isFull :: Underground -> Bool
isFull u@(Underground p _ _ _) = (geologyAt u (p+(0:+1))) == Flood
          
fillUnderground :: Underground -> Underground
fillUnderground startU = head $ dropWhile (not.isFull) $ iterate fillOneWater startU
  where fillOneWater u = processWater u $ findLast u
                                   
countWater :: Underground -> Int
countWater u@(Underground _ _ _ g) = M.size $ M.filterWithKey isCounted g
  where isCounted c v = (isWater v) && (isInBoundary u c)
                                   
countGroundwater :: Underground -> Int
countGroundwater u@(Underground _ _ _ g) = M.size $ M.filter ((==)Groundwater) g
        
{- ########### factories ########### -}

mkUnderground :: String -> Underground
mkUnderground s = finalizeUnderground (500:+0) $ groupClays $ map readClays $ lines s

finalizeUnderground :: Coordinates -> M.HashMap Coordinates Geology -> Underground
finalizeUnderground spring g = Underground spring maxY (lbound,ubound) g
  where maxY = maximum $ map y $ M.keys g
        (lbound,ubound) = ((minimum xs):+(minimum ys),(maximum xs):+(maximum ys))
        xs = [x | (x:+_) <- M.keys g]
        ys = [y | (_:+y) <- M.keys g]

groupClays :: [[Coordinates]] -> M.HashMap Coordinates Geology
groupClays = M.fromList . map (flip (,) Clay) . concat

readClays :: String -> [Coordinates]
readClays = readParams . extractFromRegex "(.)=([0-9]+), .=([0-9]+)\\..([0-9]+)"
  where readParams (dir:a:bmin:bmax:[]) = mkRange dir (read a) (read bmin) (read bmax)
        mkRange "x" x ymin ymax = [x:+y | y<-[ymin..ymax]]
        mkRange "y" y xmin xmax = [x:+y | x<-[xmin..xmax]]


extractFromRegex :: String -> String -> [String]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> subs) $ matchRegexAll (mkRegex regex) def
  
parseInput :: String -> [(Coordinates,Char)]
parseInput = filter ((/=) ' ' . snd) . concatMap parseRow . zip [0..] . lines
  where parseRow (y,s) = map (\(x,c) -> (x:+y,c)) $ zip [0..] s

{- ########### display ########### -}
showUnderground :: Underground -> String
showUnderground u@(Underground s _ ((xmin:+ymin),(xmax:+ymax)) g) = unlines $ map showRow [ y | y <- [ymin..ymax]  ]
  where withSpring = finalizeUnderground s $ M.insert s Spring g
        showRow y = map (showPoint y) [ x | x <- [(xmin-1)..(xmax+1)] ]
        showPoint y x = showGeology $ geologyAt withSpring (x:+y)
        showGeology Sand = ' '
        showGeology Clay = '#'
        showGeology Groundwater = '~'
        showGeology Flood = '|'
        showGeology Spring = '+'
