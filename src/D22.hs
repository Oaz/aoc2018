module D22
    ( Coordinate(..), Cave(..), RegionType(..)
    , mkCave, geologicIndex, erosionLevel, regionType, riskLevel, shortestPathToTarget
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.MemoCombinators as MC
import Data.Graph.Inductive
import Data.Maybe
import Data.List
import Data.Ord
import Data.Hashable

{- ######### Coordinate ######### -}
type Coordinate = (Int,Int)

rangeTo :: Coordinate -> [Coordinate]
rangeTo (xm,ym) = [(x,y) | x<-[0..xm], y<-[0..ym]]

{- ######### Cave ######### -}
data Cave = Cave  { depth :: Integer
                  , target :: Coordinate
                  , indexesAndLevels :: Int -> Int -> (Integer,Integer) }

mkCave :: Integer -> Coordinate -> Cave
mkCave depth target@(xt,yt) = Cave depth target indexesAndLevels
  where calcLevel i = mod (depth + i) 20183
        indexesAndLevels = MC.memo2 MC.integral MC.integral giel
        giel 0 0 = (0, calcLevel 0)
        giel x 0 = let i = (toInteger x)*16807
                   in (i, calcLevel i)
        giel 0 y = let i = (toInteger y)*48271
                   in (i, calcLevel i)
        giel x y = let (_,l1) = indexesAndLevels (x-1) y
                       (_,l2) = indexesAndLevels x (y-1)
                       i = l1*l2
                       i' = if (x,y) == target then 0 else i
                   in (i', calcLevel i')

rangeFor :: Cave -> [Coordinate]
rangeFor (Cave _ target _) = rangeTo target

geologicIndex :: Cave -> Coordinate -> Integer
geologicIndex (Cave _ _ il) (x,y) = let (gi,_) = il x y in gi

erosionLevel  :: Cave -> Coordinate -> Int
erosionLevel (Cave _ _ il) (x,y) = let (_,el) = il x y in fromInteger $ el

{- ######### RegionType ######### -}
data RegionType = Rocky | Wet | Narrow deriving (Show,Eq,Enum)

regionType :: Cave -> Coordinate -> RegionType
regionType c p = toEnum $ mod (erosionLevel c p) 3

instance Show Cave where 
  show cave = unlines $ map showRow [0..(snd $ target $ cave)]
    where showRow y = map (showRegion y) [0..(fst $ target $ cave)]
          showRegion y x = showRegionType $ regionType cave (x,y)
          showRegionType Rocky = '.'
          showRegionType Wet = '='
          showRegionType Narrow = '|'

{- ######### part 1 : risk level ######### -}
riskLevel :: Cave -> Int
riskLevel cave = sum $ map (fromEnum . regionType cave) $ rangeFor cave

{- ######### Tools ######### -}
data Tool = Torch | Gear | Neither deriving (Show,Eq,Enum)
instance Hashable Tool where hashWithSalt = hashUsing fromEnum

toolAllowed :: Tool -> RegionType -> Bool
toolAllowed Gear Narrow = False
toolAllowed Torch Wet = False
toolAllowed Neither Rocky = False
toolAllowed _ _ = True

allTools :: [Tool]
allTools = [Torch,Gear,Neither]

{- ######### part 2 : path search ######### -}
infix 6 :@
data Position  = Coordinate :@ Tool deriving (Eq, Show)
instance Hashable Position where 
  hashWithSalt s ((x,y):@t) = s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` t

shortestPathToTarget :: Cave -> Int
shortestPathToTarget cave@(Cave _ target _) = fromJust $ spLength 1 targetNode graph
  where graph = mkGraph nodes edges :: Gr Position Int
        nodes = listNodes cave
        edges = listEdges cave nodes
        isTargetNode (n,c:@t) = (t==Torch) && (c==target)
        targetNode = fst $ fromJust $ find isTargetNode $ nodes

listNodes :: Cave -> [(Node,Position)]
listNodes cave@(Cave _ (xt,yt) _) = zip [1..] $ concatMap positionsAt $ rangeTo (5*xt, 2*yt)
  where positionsAt c = [c:@t | t <- allTools, canUseToolAt c t]
        canUseToolAt c t = toolAllowed t $ regionType cave c

listEdges :: Cave -> [(Node,Position)] -> [(Node,Node,Int)]
listEdges cave nodes = catMaybes $ map goFromTo $ [(x1,x2) | x1<-nodes, x2<-(neighbors x1)]
  where neighbors (i,c:@t) = catMaybes [HM.lookup (c':@t') nodeSearch | c'<-neighborCoord c, t'<-allTools]
        neighborCoord (x,y) = [(x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1)]
        nodeSearch = HM.fromList $ map (\n -> (snd n,n)) nodes
        goFromTo ((i1,n1),(i2,n2)) = changePosition (i1,i2,positionChange n1 n2)
        changePosition (i1,i2,Just v) = Just (i1,i2,v)
        changePosition (i1,i2,Nothing) = Nothing

positionChange :: Position -> Position -> Maybe Int
positionChange p1@(c1:@t1) p2@(c2:@t2)
  | p1==p2 = Nothing
  | c1==c2 = Just 7
  | t1==t2 = let ((x1,y1),(x2,y2))=(c1,c2)
             in if (abs (x1-x2))+(abs (y1-y2)) == 1 then Just 1 else Nothing
  | otherwise = Nothing
