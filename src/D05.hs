module D05
    ( chainReaction, alteredChainReaction, bestAlteredChainReactionSize
    ) where

import Data.List

bestAlteredChainReactionSize :: String -> Int
bestAlteredChainReactionSize = minimum . map length . imap (map alteredChainReaction ['A'..'Z'])

imap :: [a -> b] -> a -> [b]
imap fs x = map (\f -> f x) fs

alteredChainReaction :: Char -> String -> String
alteredChainReaction prevent input = reverse $ foldl (unitReaction prevent) [] input

chainReaction :: String -> String
chainReaction = reverse . foldl (unitReaction ' ') []

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
        