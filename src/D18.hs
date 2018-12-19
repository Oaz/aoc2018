module D18
    ( Coordinates(..), Acre(..), Area(..), Counting(..)
    , mkArea, changeArea, processArea, areaValue, countVicinity, guessAreaValue
    ) where

import Data.Matrix

{- ########### Acres ########### -}
data Acre = Glade | Trees | Lumber | Void  deriving (Eq)
instance Show Acre where
  show Glade = "."
  show Trees = "|"
  show Lumber = "#"
  show Void = " "

mkAcre :: Char -> Acre
mkAcre '.' = Glade
mkAcre '|' = Trees
mkAcre '#' = Lumber  
mkAcre ' ' = Void  

{- ########### Area ########### -}

type Area = Matrix Acre
type Coordinates = (Int,Int)

mkArea :: String -> Area
mkArea s = fromList n n $ map mkAcre txt
  where n = (length $ lines s) + 2
        hf = replicate n ' '
        txt = hf ++ (concat $ map (\l -> " "++l++" ") $ lines s) ++ hf

mapMatrixWithCoordinates :: (Coordinates -> a -> b) -> Matrix a -> Matrix b
mapMatrixWithCoordinates f m = fromList nx ny $ map (\(c,a) -> f c a) $ zip range $ toList m
  where range = [(x,y) | y <- [1..ny], x <- [1..nx] ]
        nx = ncols m
        ny = nrows m

{- ########### Lumber collection ########### -}

data Counting = Zero | AtLeastOne | AtLeastThree deriving (Eq, Show)

counting :: Int -> Counting
counting 0 = Zero
counting 1 = AtLeastOne
counting 2 = AtLeastOne
counting _ = AtLeastThree

type Vicinity = (Counting,Counting)

nextAcre :: Acre -> Vicinity -> Acre
nextAcre Glade (AtLeastThree,_) = Trees 
nextAcre Glade _ = Glade 
nextAcre Trees (_,AtLeastThree) = Lumber 
nextAcre Trees _ = Trees 
nextAcre Lumber (Zero,_) = Glade 
nextAcre Lumber (_,Zero) = Glade 
nextAcre Lumber _ = Lumber 
nextAcre Void _ = Void 

countVicinity :: Area -> Coordinates -> Vicinity
countVicinity ar (x,y)
  | (getElem x y ar) == Void = (Zero,Zero)
  | otherwise                = (counting t, counting l)
  where (as1,(_:as2)) = splitAt 4 $ toList $ submatrix (y-1) (y+1) (x-1) (x+1) ar
        (t,l) = countAcres (as1++as2)

countAcres :: [Acre] -> (Int,Int)
countAcres ar = (count Trees, count Lumber)
  where count a = length $ filter ((==)a) ar

changeAcre :: Area -> Coordinates -> Acre -> Acre
changeAcre ar p ac = nextAcre ac $ countVicinity ar p

changeArea :: Area -> Area
changeArea ar = mapMatrixWithCoordinates (changeAcre ar) ar

processArea :: Int -> Area -> Area
processArea n ar = head $ drop n $ iterate changeArea ar

areaValue :: Area -> (Int,Int,Int)
areaValue ar = (t,l,t*l)
  where (t,l) = countAcres $ toList ar

guessAreaValue :: Area -> Int -> (Int,Int,Int)
guessAreaValue ar stageToGuess = areaValue equivalentArea
  where (m,mu) = floydDetection ar changeArea 
        equivalentArea = processArea (mod (stageToGuess-m) mu) $ processArea m ar
                
floydDetection :: (Eq a) => a -> (a -> a) -> (Int,Int)
floydDetection x0 f = (m,mu)
  where g (n,(x,y)) = (n+1, (f x, f $ f y))
        equals (_,(x,y)) = x == y
        findSame x = head $ dropWhile (not.equals) $ drop 1 $ iterate g (0,(x,x))
        (m,(x1,_)) = findSame x0
        (mu,_) = findSame x1
