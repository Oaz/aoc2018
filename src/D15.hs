module D15
    ( Coordinates(..), Warrior(..), Spot(..), Battlefield(..), Round(..)
    , parseInput, mkBattlefield, findWarriors, leeAlgorithm, findEnemy, findWeakest
    , startGame, playRound, hitWarrior, playGame, showBattlefield, showGame
    ) where

import Data.List
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Ord
import Data.Maybe

{- ########### Coordinates ########### -}
infix  6  :+
data Coordinates  = Int :+ Int deriving (Eq, Show)

instance Num Coordinates where
  (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
  (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
  (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
  negate (x:+y)       =  negate x :+ negate y
  abs (x:+y)          =  ((abs x) + (abs y)) :+ 0
  signum (x:+y)
      | (abs x) > (abs y) = (signum x) :+ 0
      | (abs x) < (abs y) = 0 :+ (signum y)
      | otherwise = (signum x) :+ (signum y)
  fromInteger n       =  fromInteger n :+ 0
instance Hashable Coordinates where
  hashWithSalt n (x:+y) = hashWithSalt n (x,y)
instance Ord Coordinates where
  compare (x:+y) (x':+y') = compare (y,x) (y',x')

x :: Coordinates -> Int
x (a :+ b) = a

y :: Coordinates -> Int
y (a :+ b) = b

orderedNeighbors :: Coordinates -> [Coordinates]
orderedNeighbors p = map ((+) p) [0:+(-1),(-1):+0,1:+0,0:+1]

leeAlgorithm :: Coordinates -> (Coordinates -> Bool) -> (Coordinates -> Bool) -> [Coordinates]
leeAlgorithm start isTarget isValid = if targets == [] then [] else shortestPath
  where targets = filter isTarget $ M.keys expansion :: [Coordinates]
        targetLevel = expansion M.! (head targets)
        beforeTarget = head $ sort $ withLevel (targetLevel-1) $ concatMap orderedNeighbors targets :: Coordinates
        target = head $ sort $ intersect targets (orderedNeighbors beforeTarget) :: Coordinates
        shortestPath = foldr previous [target] [1..((expansion M.! target)-1)]
        previous level ps@(p:_) = (newp:ps) where newp = head $ withLevel level $ orderedNeighbors p
        withLevel l = mapMaybe (getExp l) :: [Coordinates] -> [Coordinates]
        getExp l p = if (isJust v) && (l == fromJust v) then Just p else Nothing where v = M.lookup p expansion
        expansion = fst $ head $ filter endOfExpansion $ zip expansionFlow $ tail expansionFlow :: M.HashMap Coordinates Int
        endOfExpansion (now,lookahead) = (or $ map isTarget $ M.keys now) || (now==lookahead)
        expansionFlow = iterate expand $ M.singleton start 0
        expand m = M.foldrWithKey nextLevels m m :: M.HashMap Coordinates Int
        nextLevels coord level = M.unionWith min (nextLevel coord level)
        nextLevel coord level = M.fromList $ zip (filter isValid $ orderedNeighbors coord) (repeat (level+1)) :: M.HashMap Coordinates Int
        
leeAlgorithm' :: Coordinates -> (Coordinates -> Bool) -> (Coordinates -> Bool) -> [Coordinates]
leeAlgorithm' start isTarget isValid = if targets == [] then [] else shortestPath
  where targets = filter isTarget $ M.keys expansion :: [Coordinates]
        target = head $ sort targets :: Coordinates
        shortestPath = foldr previous [target] [1..((expansion M.! target)-1)]
        previous level ps@(p:_) = (newp:ps) where newp = head $ mapMaybe (getExp level) $ orderedNeighbors p
        getExp l p = if (isJust v) && (l == fromJust v) then Just p else Nothing where v = M.lookup p expansion
        expansion = fst $ head $ filter endOfExpansion $ zip expansionFlow $ tail expansionFlow :: M.HashMap Coordinates Int
        endOfExpansion (now,lookahead) = (or $ map isTarget $ M.keys now) || (now==lookahead)
        expansionFlow = iterate expand $ M.singleton start 0
        expand m = M.foldrWithKey nextLevels m m :: M.HashMap Coordinates Int
        nextLevels coord level = M.unionWith min (nextLevel coord level)
        nextLevel coord level = M.fromList $ zip (filter isValid $ orderedNeighbors coord) (repeat (level+1)) :: M.HashMap Coordinates Int


{- ########### Warriors ########### -}
data Warrior = Elf Int | Goblin Int deriving (Eq, Show)

enemy :: Warrior -> Warrior -> Bool
enemy (Elf _) (Elf _) = False
enemy (Goblin _) (Goblin _) = False
enemy _ _ = True

hit :: Warrior -> (Int -> Int) -> Maybe Warrior
hit (Elf hp) f = if nhp > 0 then Just (Elf nhp) else Nothing where nhp = f hp 
hit (Goblin hp) f = if nhp > 0 then Just (Goblin nhp) else Nothing where nhp = f hp

points :: Warrior -> Int
points (Elf hp) = hp
points (Goblin hp) = hp

{- ########### Spot ########### -}
data Spot = Path | Warrior Warrior deriving (Eq, Show)

isPath :: Spot -> Bool
isPath Path = True
isPath _ = False

isEnemy :: Spot -> Spot -> Bool
isEnemy Path _ = False 
isEnemy _ Path = False 
isEnemy (Warrior x) (Warrior y) = enemy x y 

getWarrior :: Spot -> Warrior
getWarrior (Warrior warrior) = warrior


{- ########### Battlefield ########### -}
type Battlefield = M.HashMap Coordinates Spot

findWarriors :: Battlefield -> [Coordinates]
findWarriors bt = sort $ M.keys $ M.filter (not.isPath) bt

findEnemy :: Battlefield -> Coordinates -> [Coordinates]
findEnemy bt pMyself = leeAlgorithm pMyself isTarget isValid
  where myself = bt M.! pMyself
        isTarget p = isEnemy myself $ bt M.! p
        isValid p = isValidEncounter $ M.lookup p bt
        isValidEncounter Nothing = False
        isValidEncounter (Just encounter) = isPath encounter || isEnemy myself encounter

findWeakest :: Battlefield -> Coordinates -> Maybe Coordinates
findWeakest bt pMyself = listToMaybe $ sortOn (points.getWarrior.((M.!) bt)) enemiesInRange
  where myself = bt M.! pMyself
        enemiesInRange = mapMaybe enemyAt $ orderedNeighbors pMyself
        enemyAt p = (fmap.const) p $ falseInNothing $ (fmap $ isEnemy myself) $ M.lookup p bt
        falseInNothing (Just True) = Just True
        falseInNothing _ = Nothing

hitWarrior :: Coordinates -> (Int -> Int) -> Battlefield -> (Battlefield, Maybe Coordinates)
hitWarrior pWarrior updateHP bt = newBattlefield newWarrior
  where newWarrior = hit (getWarrior $ bt M.! pWarrior) updateHP :: Maybe Warrior
        newBattlefield Nothing = (M.insert pWarrior Path $ M.delete pWarrior bt,Just pWarrior) :: (Battlefield, Maybe Coordinates)
        newBattlefield (Just warrior) = (M.insert pWarrior (Warrior warrior) $ M.delete pWarrior bt,Nothing) :: (Battlefield, Maybe Coordinates)

isGameOver :: Battlefield -> Bool
isGameOver bt = noEnemies $ sum $ M.elems $ M.map (mark.getWarrior) $ M.filter (not.isPath) bt
  where mark (Elf _) = 1:+0
        mark (Goblin _) = 0:+1
        noEnemies (x:+y) = (x==0) || (y==0)
  

{- ########### Rounds ########### -}
data Round = Round { roundID :: Int
                   , battlefield :: Battlefield
                   , wasFightingInPreviousTurn :: Bool
                   , warriors :: [Coordinates] } deriving (Show, Eq)

startGame :: Battlefield -> Round
startGame bt = Round 0 bt False $ findWarriors bt

playTurn :: Round -> Round
playTurn (Round n battlefield _ (pWarrior:remainingWarriorsInRound)) = Round n newBattleField fightJustOccured newRemainingWarriorsInRound
  where currentWarrior = M.lookup pWarrior battlefield
        -- moving
        moving newPos = (M.insert newPos $ fromJust currentWarrior)
                      . (M.insert pWarrior Path)
                      . (M.delete newPos)
                      . (M.delete pWarrior) :: Battlefield -> Battlefield
        warriorTurn [] = (id,pWarrior) :: (Battlefield -> Battlefield,Coordinates)
        warriorTurn (_:[]) = (id,pWarrior) :: (Battlefield -> Battlefield,Coordinates)
        warriorTurn (nextMove:_) = (moving nextMove,nextMove) :: (Battlefield -> Battlefield,Coordinates)
        (changeBtWithMove,attackCoordinates) = warriorTurn $ findEnemy battlefield pWarrior :: (Battlefield -> Battlefield,Coordinates)
        -- attacking
        warriorAttack Nothing = (\x -> (x,Nothing),False)  :: (Battlefield -> (Battlefield,Maybe Coordinates),Bool)
        warriorAttack (Just enemyPos) = (hitWarrior enemyPos (flip (-) 3),True) :: (Battlefield -> (Battlefield,Maybe Coordinates),Bool)
        warriorHits p bt = (newBt, isFighting, pDeadWarrior) :: (Battlefield,Bool,Maybe Coordinates)
          where (changeBtWithAttack,isFighting) = warriorAttack $ findWeakest bt p :: (Battlefield -> (Battlefield,Maybe Coordinates),Bool)
                (newBt, pDeadWarrior) = changeBtWithAttack bt ::(Battlefield,Maybe Coordinates)
        -- chaining move and attack
        (newBattleField,fightJustOccured,pDeadWarrior) = (warriorHits $ attackCoordinates) $ changeBtWithMove battlefield :: (Battlefield,Bool,Maybe Coordinates)
        newRemainingWarriorsInRound = remainingWarriorsInRound \\ (maybeToList pDeadWarrior) -- dead warrior won't play even if did not play in yet in round
         
playRound :: Round -> Round
playRound (Round n bt sf []) = Round (n+1) bt sf $ findWarriors bt
playRound r = playRound $ playTurn r

playGame :: String -> (Int,Int,Int)
playGame s = (rid, sumOfScores, rid*sumOfScores)
  where Round lastRoundID bt _ pRemainingWarriors = head $ dropWhile (not.isLastRound) $ runGame s
        rid = lastRoundID-1
        sumOfScores = sum $ map (points.getWarrior.((M.!) bt)) pRemainingWarriors

runGame :: String -> [Round]
runGame s = iterate playRound $ startGame $ mkBattlefield 200 $ parseInput s

isLastRound :: Round -> Bool
isLastRound (Round _ b sf _) = (isGameOver b) && (not sf)
            
{- ########### factories ########### -}
parseInput :: String -> [(Coordinates,Char)]
parseInput = concatMap parseRow . zip [0..] . lines
  where parseRow (y,s) = map (\(x,c) -> (x:+y,c)) $ zip [0..] s

mkBattlefield :: Int -> [(Coordinates,Char)] -> Battlefield
mkBattlefield hp = M.fromList . mapMaybe mkSpot  
  where mkSpot (p,'E') = Just ( p, Warrior $ Elf hp )
        mkSpot (p,'G') = Just ( p, Warrior $ Goblin hp )
        mkSpot (p,'.') = Just ( p, Path )
        mkSpot _ = Nothing
    
{- ########### display ########### -}
showBattlefield :: Battlefield -> String
showBattlefield bt = unlines $ map showRow [ r | r <- [0..(1 + (maximum $ map y allCoords))]  ]
  where allCoords = M.keys bt
        showRow r = (spots showSpot r) ++ (spots showPoints r)
        spots f r = concatMap (f.flip M.lookup bt) [ a:+r | a <- [0..(1 + (maximum $ map x allCoords))]  ]
        showPoints (Just (Warrior (Elf n))) = (' ':show n)
        showPoints (Just (Warrior (Goblin n))) = (' ':show n)
        showPoints _ = ""
        showSpot (Just Path) = "."
        showSpot (Just (Warrior (Elf _))) = "E"
        showSpot (Just (Warrior (Goblin _))) = "G"
        showSpot Nothing = "#"

showRound :: Round -> String
showRound (Round n bt _ _) = (show n) ++ "\n" ++ (showBattlefield bt)

showGame :: String -> String
showGame s = unlines $ map showRound (initial:followup)
  where rounds = runGame s
        initial = head rounds :: Round
        followup = map fst $ zip (tail rounds) (takeWhile (not.isLastRound) rounds) :: [Round]

