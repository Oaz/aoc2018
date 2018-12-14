module D14
    ( Recipes(..), move, make, search
    ) where

import Data.Char
import Data.Sequence
import Data.Foldable
import Data.Maybe

data Recipes = Recipes { elf1  :: Int
                       , elf2 :: Int
                       , scores :: Seq Char } deriving (Show, Eq)

len = Data.Sequence.length
stake = Data.Sequence.take
sdrop = Data.Sequence.drop

startRecipes :: Recipes
startRecipes = Recipes 0 1 (fromList "37")

move :: Int -> Recipes -> Recipes
move nl (Recipes e1 e2 rs) = Recipes (moveElf e1) (moveElf e2) newScores
  where currentScore = fromList $ show $ (score e1)+(score e2)
        newScores = rs >< currentScore
        moveElf e = mod (e + 1 + score e) (len newScores)
        score e = (ord $ charAt e) - (ord '0')
        charAt e = index rs e

make :: Int -> String
make n = toList $ stake 10 $ sdrop n $ head $ dropWhile ((flip (<) (n+10)) . len) $ map scores $ iterate (move 0) startRecipes

search :: Seq Char -> Int
search s = (len $ scores stopAt) - searchSize + (fromJust $ index stopAt)
  where stopAt = head $ dropWhile (not . found) $ iterate (move searchSize) startRecipes
        found r = isJust $ index r
        index (Recipes _ _ xs)
          | (endWith s xs) = Just 1
          | (endWith s ys) = Just 0
          | otherwise      = Nothing
          where (ys :|> y) = xs
        searchSize = 1 + len s

endWith :: Seq Char -> Seq Char -> Bool
endWith Empty _ = True
endWith (xs :|> x) (ys :|> y) = (x==y) && (endWith xs ys)
