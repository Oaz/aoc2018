module D22
    ( Coordinate(..), Cave(..), mkCave, geologicIndex, erosionLevel, regionType, riskLevel
    ) where

import qualified Data.MemoCombinators as MM

type Coordinate = (Int,Int)
data Cave = Cave  { depth :: Integer
                  , target :: Coordinate
                  , indexesAndLevels :: Int -> Int -> (Integer,Integer) }

rangeTo :: Coordinate -> [Coordinate]
rangeTo (xm,ym) = [(x,y) | x<-[0..xm], y<-[0..ym]]

mkCave :: Integer -> Coordinate -> Cave
mkCave depth target = Cave depth target indexesAndLevels
  where calcLevel i = mod (depth + i) 20183
        indexesAndLevels = MM.memo2 MM.integral MM.integral giel
        giel 0 0 = (0, calcLevel 0)
        giel x 0 = let i = (toInteger x)*16807
                   in (i, calcLevel i)
        giel 0 y = let i = (toInteger y)*48271
                   in (i, calcLevel i)
        giel x y = let (_,l1) = indexesAndLevels (x-1) y
                       (_,l2) = indexesAndLevels x (y-1)
                       i = l1*l2
                   in (i, calcLevel i)

geologicIndex :: Cave -> Coordinate -> Integer
geologicIndex (Cave _ _ il) (x,y) = let (gi,_) = il x y in gi

erosionLevel  :: Cave -> Coordinate -> Int
erosionLevel (Cave _ _ il) (x,y) = let (_,el) = il x y in fromInteger $ el

regionType :: Cave -> Coordinate -> Int
regionType c p = mod (erosionLevel c p) 3

riskLevel :: Cave -> Int
riskLevel c@(Cave _ p@(xt,yt) il) = s - regionType c p
  where s = sum $ map (regionType c) [(x,y) | x<-[0..xt], y<-[0..yt]]