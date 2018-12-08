module D08
    ( Cursor(..), moveCursor, Block(..), metadataSum, metadataSumByIndex, parseLicense
    ) where

import Data.List
import Data.Tree

type LicenseMetadata = [Int]

data Block = Block { remainingChildren :: Int,
                     nbMetadata :: Int,
                     doneChildren :: Forest LicenseMetadata } deriving (Show, Eq)

data Cursor = Cursor { stack :: [Block],
                       stream :: [Int] } deriving (Show, Eq)

moveCursor :: Cursor -> Cursor
moveCursor (Cursor [] (nbc:nbm:ints))                           = Cursor [Block nbc nbm []] ints
moveCursor (Cursor ((Block 0 nbm children):parent:blocks) ints) = Cursor ((processNode parent (take nbm ints) children):blocks) (drop nbm ints)
  where processNode (Block nbc nbm siblings) metadata children = Block nbc nbm ((Node metadata $ reverse children):siblings)
moveCursor (Cursor [Block 0 nbm children] ints)                 = Cursor [Block 0 0 [Node ints $ reverse children]] (drop nbm ints)
moveCursor (Cursor (top:blocks) (0:nbm:ints))                   = Cursor ((processLeaf top (take nbm ints)):blocks) (drop nbm ints)
  where processLeaf (Block nbc nbm children) metadata = Block (nbc - 1) nbm ((Node metadata []):children)
moveCursor (Cursor (top:blocks) (nbc:nbm:ints))                 = Cursor ((Block nbc nbm []):top { remainingChildren = (remainingChildren top) - 1 }:blocks) ints

metadataSum :: Tree LicenseMetadata -> Int
metadataSum = foldTree (\a b -> sum $ a ++ b)

metadataSumByIndex :: Tree LicenseMetadata -> Int
metadataSumByIndex (Node m []) = sum m
metadataSumByIndex (Node m c) = sum $ map (byIndexOn c) m
  where byIndexOn children index
          | (index > 0) && (index <= (length children)) = metadataSumByIndex (children !! (index-1))
          | otherwise = 0

parseLicense :: String -> Tree LicenseMetadata
parseLicense = head . doneChildren . head . stack . head . dropWhile ((>0) . length . stream) . iterate moveCursor . Cursor [] . map read . words
