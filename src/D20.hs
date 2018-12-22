module D20
    ( largestNumberOfDoors, countRoomsAfterDoors
    ) where

import Text.Parsec
import Data.Functor.Identity
import Data.Either
import Data.Complex
import qualified Data.HashMap.Strict as M

type Position = Complex Float
data Navigation = Navigation  { currentPosition :: Position
                              , pastBranches :: [Position]
                              , distances :: M.HashMap Position Int } deriving (Show,Eq)

changePosition :: Position -> Navigation -> Navigation
changePosition p (Navigation cp pb d) = Navigation np pb (M.insert np (shortest (M.lookup np d) (M.lookup cp d)) d)
  where np=cp+p
        shortest Nothing (Just n) = n+1
        shortest (Just n) Nothing = n
        shortest (Just m) (Just n) = min m (n+1)

enterBranch :: Navigation -> Navigation
enterBranch (Navigation cp pb d) = Navigation cp (cp:pb) d

tryAlternateBranch :: Navigation -> Navigation
tryAlternateBranch (Navigation _ pb@(p:_) d) = Navigation p pb d

leaveBranch :: Navigation -> Navigation
leaveBranch (Navigation _ (p:pb) d) = Navigation p pb d

type RoomParser = ParsecT String Navigation Identity

tryAllPaths :: ParsecT String Navigation Identity Navigation
tryAllPaths = char '^'
              >> (many $ choice [ char 'N' >> modifyState (changePosition $ 0:+(-1))
                                , char 'S' >> modifyState (changePosition $ 0:+1)
                                , char 'E' >> modifyState (changePosition $ 1:+0)
                                , char 'W' >> modifyState (changePosition $ (-1):+0)
                                , char '(' >> modifyState enterBranch
                                , char '|' >> modifyState tryAlternateBranch
                                , char ')' >> modifyState leaveBranch ])
              >> char '$'
              >> getState

getAllDistances :: String -> [Int]
getAllDistances = M.elems . distances . fromRight nav . runParser tryAllPaths nav ""
  where nav = Navigation (0:+0) [] (M.singleton (0:+0) 0)

largestNumberOfDoors :: String -> Int
largestNumberOfDoors = maximum . getAllDistances

countRoomsAfterDoors :: Int -> String -> Int
countRoomsAfterDoors threshold = length . filter (((<=)threshold)) . getAllDistances

