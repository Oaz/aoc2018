module D18
    ( Coordinates(..), Acre(..), Area(..)
    , parseInput, mkArea, changeArea, processArea, areaValue
    , showArea
    ) where

import Data.List
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Ord

{- ########### Coordinates ########### -}
infix  6  :+
data Coordinates  = Int :+ Int deriving (Eq, Show)

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
instance Hashable Coordinates where
  hashWithSalt n (x:+y) = hashWithSalt n (x,y)
instance Ord Coordinates where
  compare (x:+y) (x':+y') = compare (y,x) (y',x')

x :: Coordinates -> Int
x (a :+ b) = a

y :: Coordinates -> Int
y (a :+ b) = b

{- ########### Lumber collection ########### -}
data Acre = Glade | Trees | Lumber  deriving (Eq, Show)
instance Hashable Acre where
  hashWithSalt n Glade = 0
  hashWithSalt n Trees = 1
  hashWithSalt n Lumber = 2

type Area = M.HashMap Coordinates Acre

data Counting = Zero | AtLeastOne | AtLeastThree deriving (Eq, Show)
type Vicinity = (Counting,Counting)

counting :: Int -> Counting
counting 0 = Zero
counting 1 = AtLeastOne
counting 2 = AtLeastOne
counting _ = AtLeastThree

nextAcre :: Acre -> Vicinity -> Acre
nextAcre Glade (AtLeastThree,_) = Trees 
nextAcre Glade (_,_) = Glade 
nextAcre Trees (_,AtLeastThree) = Lumber 
nextAcre Trees (_,_) = Trees 
nextAcre Lumber (Zero,_) = Glade 
nextAcre Lumber (_,Zero) = Glade 
nextAcre Lumber (_,_) = Lumber 

countVicinity :: Area -> Coordinates -> Vicinity
countVicinity ar p = (counting t, counting l)
  where coords = map ((+)p) [x:+y | x <-[(-1)..1], y<-[(-1)..1], (x,y)/=(0,0) ]
        (t,l) = countAcres $ M.filterWithKey (\k _ -> elem k coords) ar

countAcres :: Area -> (Int,Int)
countAcres ar = (count Trees, count Lumber)
  where counts = M.fromListWith (+) [ (a, 1) | a <- M.elems ar ]
        count x = countMaybe $ M.lookup x counts
        countMaybe Nothing = 0
        countMaybe (Just n) = n

changeAcre :: Area -> Coordinates -> Acre -> Acre
changeAcre ar p ac = nextAcre ac $ countVicinity ar p

changeArea :: Area -> Area
changeArea ar = M.mapWithKey (changeAcre ar) ar

processArea :: Int -> Area -> Area
processArea n ar = head $ drop n $ iterate changeArea ar

areaValue :: Area -> (Int,Int,Int)
areaValue ar = (t,l,t*l)
  where (t,l) = countAcres ar

{- ########### factories ########### -}
parseInput :: String -> [(Coordinates,Char)]
parseInput = concatMap parseRow . zip [0..] . lines
  where parseRow (y,s) = map (\(x,c) -> (x:+y,c)) $ zip [0..] s

mkArea :: [(Coordinates,Char)] -> Area
mkArea = M.fromList . map mkAcre  
  where mkAcre (p,'.') = (p,Glade)
        mkAcre (p,'|') = (p,Trees)
        mkAcre (p,'#') = (p,Lumber)
    
{- ########### display ########### -}
showArea :: Area -> String
showArea a = unlines $ map showRow [ r | r <- [(minimum ys)..(maximum ys)]  ]
  where allCoords = M.keys a
        showRow y = map (showAcre.((M.!) a)) [ x:+y | x <- [(minimum xs)..(maximum xs)]  ]
        showAcre Glade = '.'
        showAcre Trees = '|'
        showAcre Lumber = '#'
        (xs,ys) = (map x allCoords, map y allCoords)
