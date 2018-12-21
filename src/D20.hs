module D20
    ( largestNumberOfDoors
    ) where

import Text.Parsec
import Data.Functor.Identity
import Data.Either

type DoorsParser = ParsecT String () Identity Int

largestNumberOfDoors :: String -> Int
largestNumberOfDoors = fromRight 0 . runParser directions () ""

directions :: DoorsParser
directions = do { char '^'
                ; numberOfDoors <- path
                ; char '$'
                ; return numberOfDoors
                }

path :: DoorsParser
path = do { directions <- many $ oneOf "NSEW"
          ; followedBy <- option 0 (do { char '('
                                        ; paths <- path `sepBy` (char '|')
                                        ; char ')'
                                        ; followedBy <- option 0 path
                                        ; return $ (choosePath paths) + followedBy
                                        })
          ; return $ (length directions) + followedBy
          }
  where choosePath xs = if elem 0 xs then 0 else maximum xs
