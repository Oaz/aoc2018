module D02
    ( boxesChecksum, commonLetters
    ) where

import Data.List

boxesChecksum :: String -> Int
boxesChecksum = checksum . foldl1 add . map has2or3 . map occurrences . words
  where
    checksum (a,b) = a*b
    add (x,y) (x',y') = (x+x',y+y')
    has2or3 xs = (boolint $ elem 2 xs, boolint $ elem 3 xs)
    boolint x = if x then 1 else 0
    occurrences = map length . group .sort

commonLetters :: String -> String
commonLetters = common . correctIDs . words
  where
    common (xs,ys) = map fst $ filter same $ zip xs ys
    correctIDs ids = head $ snd $ break differByOne [(x,y) | x <- ids, y <- ids]
    differByOne (xs,ys) = 1 == (sum $ map distance $ zip xs ys)
    distance x = if same x then 0 else 1
    same (a,b) = a==b

