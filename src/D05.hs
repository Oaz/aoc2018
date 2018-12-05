module D05
    ( chainReaction, alteredChainReaction, bestAlteredChainReactionSize
    ) where

import Data.List
import Control.Applicative

bestAlteredChainReactionSize :: String -> Int
bestAlteredChainReactionSize = minimum . map length . (<*>) (map alteredChainReaction ['A'..'Z']) . (:[])

alteredChainReaction :: Char -> (String -> String)
alteredChainReaction prevent = reverse . foldl (unitReaction prevent) []

chainReaction :: String -> String
chainReaction = alteredChainReaction ' '

unitReaction :: Char -> String -> Char -> String
unitReaction p [] y = if (isSameType y p) then [] else [y]
unitReaction p xs@(x:s) y
    | isSameType y p = xs
    | opposites x y = s
    | otherwise = y:xs

isSameType :: Char -> Char -> Bool
isSameType x y = (x == y) || (opposites x y)

opposites :: Char -> Char -> Bool
opposites x y = 32 == (abs $ fromEnum x - fromEnum y)
        