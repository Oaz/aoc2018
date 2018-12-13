module D13
    ( parseInput, Coordinates(..), parseWorld, Ground, Road(..), Path(..)
    , Fleet(..), cartsToFleet, Cart(..), Navigation(..), Direction(..), nextCart
    , forward, findCrash, avoidCrash
    ) where

import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Ord

{- ########### Coordinates ########### -}
infix  6  :+
data Coordinates  = Int :+ Int deriving (Eq, Show)
instance Hashable Coordinates where
  hashWithSalt n (x:+y) = hashWithSalt n (x,y)
instance Ord Coordinates where
  compare (x:+y) (x':+y') = compare (y,x) (y',x')
  
{- ########### Cart ########### -}
data Path = StraightNS | StraightEW | CurveSlash | CurveAnti | Cross deriving (Show, Eq)
data Direction = North | South | East | West deriving (Show, Eq)
data Navigation = GoLeft | GoStraight | GoRight deriving (Show, Eq)
data Cart = Cart { position :: Coordinates
                 , direction :: Direction
                 , navigation :: Navigation } deriving (Show, Eq)

nextPosition :: Cart -> Coordinates
nextPosition (Cart (x:+y) North _) = x:+(y-1)
nextPosition (Cart (x:+y) South _) = x:+(y+1)
nextPosition (Cart (x:+y) East _) = (x+1):+y
nextPosition (Cart (x:+y) West _) = (x-1):+y

nextDirection :: Path -> Cart -> Direction
nextDirection StraightNS (Cart _ North _) = North
nextDirection StraightNS (Cart _ South _) = South
nextDirection StraightEW (Cart _ East _) = East
nextDirection StraightEW (Cart _ West _) = West
nextDirection CurveSlash (Cart _ North _) = East
nextDirection CurveSlash (Cart _ East _) = North
nextDirection CurveSlash (Cart _ South _) = West
nextDirection CurveSlash (Cart _ West _) = South
nextDirection CurveAnti (Cart _ North _) = West
nextDirection CurveAnti (Cart _ West _) = North
nextDirection CurveAnti (Cart _ South _) = East
nextDirection CurveAnti (Cart _ East _) = South
nextDirection Cross (Cart _ North GoLeft) = West
nextDirection Cross (Cart _ North GoStraight) = North
nextDirection Cross (Cart _ North GoRight) = East
nextDirection Cross (Cart _ South GoLeft) = East
nextDirection Cross (Cart _ South GoStraight) = South
nextDirection Cross (Cart _ South GoRight) = West
nextDirection Cross (Cart _ East GoLeft) = North
nextDirection Cross (Cart _ East GoStraight) = East
nextDirection Cross (Cart _ East GoRight) = South
nextDirection Cross (Cart _ West GoLeft) = South
nextDirection Cross (Cart _ West GoStraight) = West
nextDirection Cross (Cart _ West GoRight) = North

nextNavigation :: Navigation -> Navigation
nextNavigation GoLeft = GoStraight
nextNavigation GoStraight = GoRight
nextNavigation GoRight = GoLeft

nextCartNavigation :: Path -> Cart -> Navigation
nextCartNavigation Cross (Cart _ _ n) = nextNavigation n
nextCartNavigation _     (Cart _ _ n) = n

{- ########### Ground ########### -}
data Road = Road { situation :: Coordinates
                 , path :: Path } deriving (Show, Eq)
type Ground = M.HashMap Coordinates Road

nextCart :: Ground -> Cart -> Cart
nextCart ground cart = Cart newPos (nextDirection newPath cart) (nextCartNavigation newPath cart)
  where newPos = nextPosition cart
        newPath = path (ground M.! newPos)

{- ########### Fleet ########### -}
data Fleet = Fleet { allCarts :: M.HashMap Coordinates Cart
                   , remain :: [Cart]
                   , crash :: Maybe Coordinates } deriving (Show, Eq)

cartsToFleet :: [Cart] -> Fleet
cartsToFleet carts = Fleet (M.fromList $ map (\c -> (position c,c)) carts) (sortBy (comparing position) carts) Nothing

forward :: Ground -> Fleet -> Fleet
forward ground (Fleet allCarts (currentCart:remainingCarts) _) = Fleet newAllCarts newRemainingCarts crashCoordinates
  where newCart@(Cart newPos _ _) = nextCart ground currentCart
        allCartsAfterMove = M.insert newPos newCart $ M.delete (position currentCart) allCarts
        crashCoordinates
          | M.member newPos allCarts = Just newPos
          | otherwise                = Nothing
        newAllCarts
          | crashCoordinates == Nothing = allCartsAfterMove
          | otherwise                   = M.delete newPos allCartsAfterMove
        newRemainingCarts
          | remainingCarts == []        = sortBy (comparing position) $ M.elems newAllCarts
          | crashCoordinates == Nothing = remainingCarts
          | otherwise                   = filter (not.((==)newPos).position) remainingCarts

{- ########### crashes ########### -}
findCrash :: (Ground,Fleet) -> Coordinates
findCrash (ground,fleet) = head $ mapMaybe crash $ iterate (forward ground) fleet

avoidCrash :: (Ground,Fleet) -> Coordinates
avoidCrash (ground,fleet) = head $ mapMaybe snd $ iterate (check . forward ground . fst) (fleet,Nothing)
  where check f@(Fleet a _ _) = (f, if (M.size a) == 1 then (Just $ head $ M.keys a) else Nothing)
             
{- ########### factories ########### -}
parseInput :: String -> [(Coordinates,Char)]
parseInput = filter ((/=) ' ' . snd) . concatMap parseRow . zip [0..] . lines
  where parseRow (y,s) = map (\(x,c) -> (x:+y,c)) $ zip [0..] s

mkGround :: [(Coordinates,Char)] -> Ground
mkGround = M.fromList . map mkRoad  
  where mkRoad (p,c) = ( p, Road p $ charToPath c )
          where charToPath '|' = StraightNS
                charToPath '^' = StraightNS
                charToPath 'v' = StraightNS
                charToPath '-' = StraightEW
                charToPath '<' = StraightEW
                charToPath '>' = StraightEW
                charToPath '/' = CurveSlash
                charToPath '\\' = CurveAnti
                charToPath '+' = Cross

mkFleet :: [(Coordinates,Char)] -> Fleet
mkFleet = cartsToFleet . map mkCart . mapMaybe findDirection  
  where mkCart (p,d) = Cart p d GoLeft
        findDirection (p,'^') = Just (p,North)
        findDirection (p,'v') = Just (p,South)
        findDirection (p,'<') = Just (p,West)
        findDirection (p,'>') = Just (p,East)
        findDirection (_,_)   = Nothing

parseWorld :: String -> (Ground,Fleet)
parseWorld s = (mkGround p,mkFleet p)
  where p = parseInput s
