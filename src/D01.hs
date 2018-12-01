module D01
    ( resultingFrequency, frequencyReachedTwice, firstTwice
    ) where

import Data.List
import Data.List.Utils
import qualified Data.HashSet as H

frequencyReachedTwice :: String -> Int
frequencyReachedTwice = firstTwice . scanl (+) 0 . cycle . frequencyChanges

firstTwice :: [Int] -> Int
firstTwice items = fst $ head $ snd $ break (\(x,xs) -> H.member x xs) $ zip items $ scanl (flip H.insert) H.empty items

resultingFrequency :: String -> Int
resultingFrequency = sum . frequencyChanges

frequencyChanges :: String -> [Int]
frequencyChanges = map readInt . words

readInt :: String -> Int
readInt = read . replace "+" ""

