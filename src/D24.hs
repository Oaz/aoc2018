module D24
    ( AttackType(..), Purpose(..), Powers(..), Group(..), Army(..)
    , make, anAttackType, aListOfAttackTypes, somePowers, aGroup, anArmy, aFight
    , withID, withID2, comparing2, comparing3
    , GIA(..), Fight(..), Attack(..)
    , mkBattlefield, targetSelection, resolveAttack, unrollWar
    , findMinimalBoost, boost
    ) where

import Text.Parsec
import Text.ParserCombinators.Parsec.Token
import Data.Functor.Identity
import Data.Either
import Data.Char
import Data.Maybe
import Data.List
import Data.Ord

{- ######### armies definitions ######### -}

data Army = Army { name :: String
                 , groups :: [Group] } deriving (Eq, Show)

data Group = Group { units :: Int
                   , powers :: Powers } deriving (Eq, Show)

effectivePower :: Group -> Int
effectivePower (Group u p) = u * (damage p)

data Powers = Powers { hitPoints :: Int
                     , attackType :: AttackType
                     , damage :: Int
                     , initiative :: Int
                     , weaknesses :: [AttackType]
                     , immunities :: [AttackType] } deriving (Eq, Show)

data AttackType = Slashing | Fire | Radiation | Bludgeoning | Cold deriving (Eq, Show)

damageFactor :: Powers -> Powers -> Int
damageFactor attacker attackee
  | elem at (immunities attackee) = 0
  | elem at (weaknesses attackee) = 2
  | otherwise                     = 1
  where at = attackType attacker

theoriticalDamage :: Group -> Group -> Int
theoriticalDamage ga@(Group _ pa) (Group _ pd) = (damageFactor pa pd) * (effectivePower ga)
        
executeAttack :: Group -> Group -> Maybe Group
executeAttack g1 g2 = if remainingUnits > 0 then Just $ g2 { units = remainingUnits } else Nothing
  where lostUnits = div (theoriticalDamage g1 g2) (hitPoints $ powers g2)
        remainingUnits = (units g2) - lostUnits

{- ######### building an army ######### -}

type ArmyParser = ParsecT String Int Identity
data Purpose = Weaken | Immunize deriving (Eq, Show)

anAttackType :: ArmyParser AttackType
anAttackType = choice [ string "slashing" >> return Slashing
                      , string "fire" >> return Fire
                      , string "radiation" >> return Radiation
                      , string "bludgeoning" >> return Bludgeoning
                      , string "cold" >> return Cold ]

aListOfAttackTypes :: ArmyParser (Purpose,[AttackType])
aListOfAttackTypes = do { purpose <- choice [ string "weak" >> return Weaken
                                            , string "immune" >> return Immunize ]
                        ; string " to "
                        ; attackTypes <- sepBy1 anAttackType (string ", ")
                        ; return (purpose,attackTypes)
                        }

aNumber :: ArmyParser Int
aNumber = do  { digits <- many1 digit
              ; let n = foldl (\x d -> 10*x + digitToInt d) 0 digits
              ; seq n (return n)
              }

somePowers :: ArmyParser Powers
somePowers = do { string "each with "
                ; hitPoints <- aNumber
                ; string " hit points"
                ; hpModifiers <- option [] $ try 
                                  $ between (string " (")
                                            (string ")")
                                            $ sepBy1 aListOfAttackTypes (string "; ")
                ; let weaknesses = fromMaybe [] $ lookup Weaken hpModifiers
                ; let immunities = fromMaybe [] $ lookup Immunize hpModifiers
                ; string " with an attack that does "
                ; damage <- aNumber
                ; string " "
                ; attackType <- anAttackType
                ; string " damage at initiative "
                ; initiative <- aNumber
                ; return $ Powers hitPoints attackType damage initiative weaknesses immunities
                }

aGroup :: ArmyParser Group
aGroup = do { units <- aNumber
            ; string " units "
            ; powers <- somePowers
            ; return $ Group units powers
            }

anArmy :: ArmyParser Army
anArmy = do { name <- many1 $ choice [alphaNum,char ' ']
            ; char ':'
            ; newline
            ; groups <- endBy1 aGroup newline
            ; return $ Army name groups
            }

aFight :: ArmyParser [Army]
aFight = sepBy1 anArmy newline

make :: ArmyParser a -> String -> Either ParseError a
make p i = runParser p 0 "" i

{- ######### ID adders ######### -}

withID :: [a] -> [(Int,a)]
withID = zip [1..]

withID2 :: (a -> [b]) -> [a] -> [(Int,Int,b)]
withID2 f =  map flatten . withID . concatMap toB . withID
  where toB (aID,a) = map ((,) aID) $ f a
        flatten (bID,(aID,b)) = (aID,bID,b)

{- ######### ord composition ######### -}

comparing2 :: (Ord b,Ord c) => (a->b) -> (a->c) -> a -> a -> Ordering
comparing2 p1 p2 = comparingComp p1 $ comparing p2

comparing3 :: (Ord b,Ord c,Ord d) => (a->b) -> (a->c) -> (a->d) -> a -> a -> Ordering
comparing3 p1 p2 p3 = comparingComp p1 $ comparing2 p2 p3

comparingComp :: (Ord b) => (a->b) -> (a -> a -> Ordering) -> a -> a -> Ordering
comparingComp p comp x y
  | c == GT   = GT
  | c == LT   = LT
  | otherwise = comp x y
  where c = comparing p x y

{- ######### misc utils ######### -}

validIf :: (a -> Bool) -> a -> Maybe a
validIf f x = if f x then Just x else Nothing

replaceBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
replaceBy f x0 = map (\x -> if (f x x0) == EQ then x0 else x)

bisect :: (Int -> Ordering) -> Int -> Int -> (Int,Int)
bisect f xmin xmax = compute xmin xmax
  where compute x0 x1 = let m = div (x0+x1) 2 in further x0 x1 (x1-x0) m (f m)
        further x0 x1 1 _  _  = (x0,x1)
        further x0 _  _ xm GT = compute x0 xm
        further _  x1 _ xm _  = compute xm x1
      
{- ######### part 1 - fighting ######### -}

data GIA = GIA { armyID ::Int
               , groupID ::Int
               , def ::Group } deriving (Eq, Show)

onGIA :: (Group -> a) -> (GIA -> a)
onGIA f = let h (GIA _ _ g) = f g in h

onGIA2 :: (Group -> Group -> a) -> (GIA -> GIA -> a)
onGIA2 f = let h (GIA _ _ g1) (GIA _ _ g2) = f g1 g2 in h

data Attack = A { attacker ::Int
                , attackee ::Int
                , position ::Int } deriving (Eq, Show)
data Fight = F [GIA] [Attack] deriving (Eq, Show)

mkBattlefield :: [Army] -> [GIA]
mkBattlefield armies =  map mkGIA $ withID2 groups armies
  where mkGIA (aID,gID,g) = GIA aID gID g
        
targetSelection :: [GIA] -> Fight
targetSelection gias = chooseFrom (attackersBySelectionPriority, gias, [])
  where attackersBySelectionPriority = sortBy (comparing2 (Down . onGIA effectivePower)
                                                          (Down . onGIA (initiative.powers)) ) gias
        chooseFrom ((gia:gs),remainingTargets,chosenAttacks) =
                select gia (chooseTarget gia remainingTargets) (gs,remainingTargets,chosenAttacks)
        chooseFrom ([],_,chosenAttacks) = F gias chosenAttacks
        select _ Nothing r = chooseFrom r
        select gia (Just t) (gs,remainingTargets,chosenAttacks) =
                            chooseFrom (gs,remainingTargets \\ [t],(mkAttack gia t):chosenAttacks)
        mkAttack (GIA _ id1 g1) (GIA _ id2 _) = A id1 id2 (initiative $ powers g1)

chooseTarget :: GIA -> [GIA] -> Maybe GIA
chooseTarget (GIA aID _ g) = (=<<) (validIf ((/=0).myDamage))
                             . listToMaybe
                             . sortBy (comparing3 (Down . myDamage)
                                                  (Down . onGIA effectivePower)
                                                  (Down . onGIA (initiative.powers)) )
                             . filter ((/=aID).armyID)
  where myDamage = onGIA $ theoriticalDamage g

resolveAttack :: Fight -> [GIA]
resolveAttack (F startGias atks) = foldr addAttack startGias $ sortBy (comparing position) atks
  where addAttack (A attack defense _) gias = executeAttackGIA gias (giaByID gias attack) (giaByID gias defense)
        giaByID gias giaID = find ((==giaID).groupID) gias

executeAttackGIA :: [GIA] -> Maybe GIA -> Maybe GIA -> [GIA]
executeAttackGIA gias _           Nothing     = gias -- nothing to do if attackee already killed
executeAttackGIA gias Nothing     _           = gias -- nothing happens if attacker already killed
executeAttackGIA gias (Just gia1) (Just gia2) = processAttackResult $ fmap wrap $ (onGIA2 executeAttack) gia1 gia2
  where wrap g = gia2 { def = g }
        processAttackResult Nothing    = gias \\ [gia2]                         -- remove attackee if it was just killed
        processAttackResult (Just gia) = replaceBy (comparing groupID) gia gias -- replace attackee if it lost some units

unrollWar :: [Army] -> Int
unrollWar = sum . map (units.def) . head . filter hasWinner . iterate (resolveAttack.targetSelection) . mkBattlefield

hasWinner :: [GIA] -> Bool
hasWinner gias = (head aids)*(length aids) == (sum aids)
  where aids = map armyID gias

{- ######### part 2 - boost ######### -}

boost :: [Int] -> [Army] -> [Army]
boost bs armies = map boostArmy $ zip bs armies
  where boostArmy (b,(Army name groups)) = Army name $ map (boostGroup b) groups
        boostGroup b (Group units powers) = Group units $ powers { damage = (damage powers) + b }

findMinimalBoost :: [Army] -> Int
findMinimalBoost armies = b1
  where hb = sum $ map (damage.powers) $ concatMap groups armies
        (b0,b1) = bisect (tryBoost armies) 0 hb

tryBoost :: [Army] -> (Int -> Ordering)
tryBoost armies = withBoost
  where play gias = let rs = iterate (resolveAttack.targetSelection) gias in head $ filter winOrStale $ zip rs $ tail rs
        winOrStale (gias0,gias1) = (hasWinner gias1) || (gias0 == gias1)
        winnerID (gias0,gias1) = if hasWinner gias1 then armyID $ head gias1 else 0
        withBoost b = if (winnerID $ play $ mkBattlefield $ boost [b,0] armies) == 1 then GT else LT
