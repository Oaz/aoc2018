module D11bis
    ( powerCells, summedAreaTable, highest, ultimateHighest
    ) where

import qualified Data.List as L
import qualified Data.Matrix as M
import Data.Ord

powerCells :: Int -> M.Matrix Int
powerCells seed = M.matrix 300 300 cellValue
  where cellValue (x,y) = (flip (-)) 5 $ digit 100 $ (rackId*y+seed)*rackId
          where rackId = x+10
                digit n v = mod (div v n) 10

summedAreaTable :: (Num a) => M.Matrix a -> M.Matrix a
summedAreaTable = M.transpose . sumRows . M.transpose . sumRows
  where sumRows m = L.foldl (\m' r -> M.combineRows r 1 (r-1) m') m [2..(M.nrows m)]

highest :: Int -> M.Matrix Int -> ((Int,Int),Int)
highest size sat = L.maximumBy (comparing snd) [((x+1,y+1),sumBySat x y) | x<-range, y<-range]
  where range = [0..(300-size)]
        sumBySat x y = (val x' y') + (val x y) - (val x' y) - (val x y')
          where x' = x+size
                y' = y+size
                val 0 _ = 0
                val _ 0 = 0
                val a b = M.getElem a b sat

ultimateHighest :: M.Matrix Int -> ((Int,Int,Int),Int)
ultimateHighest sat = L.maximumBy (comparing snd) cells
  where cells = L.concat [[((x,y,size),v) | ((x,y),v)<-[highest size sat]] | size<-[1..300]]
                                        

