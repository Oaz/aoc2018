module D11
    ( powerCells, highestPowerCell, ultimateHighestPowerCell
    ) where

import Data.List
import Data.Matrix
import Data.Ord

powerCells :: Int -> Matrix Int
powerCells seed = matrix 300 300 cellValue
  where cellValue (x,y) = (flip (-)) 5 $ digit 100 $ (rackId*y+seed)*rackId
          where rackId = x+10
                digit n v = mod (div v n) 10

highestPowerCell :: Int -> Int -> ((Int,Int),Int)
highestPowerCell size seed = maximumBy (comparing snd) [((x,y),sumPower x y) | x<-range, y<-range]
  where sumPower x y = sum $ toList $ submatrix x (x+size-1) y (y+size-1) $ powerCells seed
        range = [1..(301-size)]

ultimateHighestPowerCell :: Int -> ((Int,Int,Int),Int)
ultimateHighestPowerCell seed = maximumBy (comparing snd) cells
  where cells = concat [[((x,y,size),v) | ((x,y),v)<-[highestPowerCell size seed]] | size<-[1..16]]
                                        

