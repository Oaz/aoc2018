module D14bis
    ( Recipes(..), startRecipes, index1, index2, toSeq, move, make, search
    ) where

import Prelude hiding (length,take,drop)
import Data.Char
import Data.Sequence
import Data.Foldable hiding (length)
import Data.Maybe

data Recipes = Recipes { before :: Seq Char
                       , elf1 :: Char
                       , between :: Seq Char
                       , elf2 :: Char
                       , after :: Seq Char } deriving (Show, Eq)

startRecipes :: Recipes
startRecipes = Recipes Empty '3' Empty '7' Empty

move :: Recipes -> Recipes
move r@(Recipes _ e1 _ e2 _) = foldl (flip ($)) withScore transformations 
  where withScore = append r $ score e1 e2
        transformations = concatMap (\(f,n) -> Prelude.replicate n f) $ Prelude.zip [shift12R,shift2R,shift1R] $ findShifts withScore

findShifts :: Recipes -> [Int]
findShifts r@(Recipes _ e1 _ e2 _) = defShift (signum (d2-d1)) (signum (l2-l1)) (signum (k2-k1))
  where (i1,i2) = (index1 r, index2 r)
        s = size r
        adjust n = mod n s
        (m1,m2) = (adjust $ 1 + charValue e1, adjust $ 1 + charValue e2)
        (d1,d2) = if i1+m1 < i2+m2 then (m1,m2) else (m2+i2-i1,m1+i1-i2) -- switch roles if 1st elf goes beyond 2nd elf
        shiftBoth = if d1 < d2 then d1 else d2
        (k1,k2) = (adjust $ i1+shiftBoth, adjust $ i2+shiftBoth)
        (l1,l2) = (adjust $ i1+d1, adjust $ i2+d2)
        defShift 1 1 _  = [d1, d2-d1,     0]
        defShift 1 _ 1  = [d1,  s-k2,    l2]
        defShift 1 _ _  = [d1,     0, d2-d1]
        defShift _ _ 1  = [d2,     0, d1-d2]
        defShift _ _ _  = [d2, d1-d2,     0]
           
charValue :: Char -> Int
charValue c = (ord c) - (ord '0')

score :: Char -> Char -> String
score e1 e2 = show $ (charValue e1)+(charValue e2)

index1 :: Recipes -> Int
index1 (Recipes bf _ _ _ _) = length bf

index2 :: Recipes -> Int
index2 (Recipes bf _ bt _ _) = 1 + (length bf) + (length bt)

size :: Recipes -> Int
size (Recipes bf _ bt _ af) = 2 + (length bf) + (length bt) + (length af)

append :: Recipes -> [Char] -> Recipes
append (Recipes bf e1 bt e2 af) cs = (Recipes bf e1 bt e2 (af >< (fromList cs)))

shift12R :: Recipes -> Recipes
shift12R (Recipes (n1:<|bf) e1 Empty e2 Empty) = Recipes Empty n1 (bf|>e1) e2 Empty
shift12R (Recipes bf e1 Empty e2 (n2:<|af)) = Recipes (bf|>e1) e2 Empty n2 af
shift12R (Recipes Empty e1 (n1:<|bt) e2 Empty) = Recipes Empty e1 Empty n1 (bt|>e2)
shift12R (Recipes (n1:<|bf) e1 (n2:<|bt) e2 Empty) = Recipes Empty n1 (bf|>e1) n2 (bt|>e2)
shift12R (Recipes bf e1 (n1:<|bt) e2 (n2:<|af)) = Recipes (bf|>e1) n1 (bt|>e2) n2 af

shift1R :: Recipes -> Recipes
shift1R (Recipes bf e1 (n1:<|bt) e2 af) = Recipes (bf|>e1) n1 bt e2 af

shift2R :: Recipes -> Recipes
shift2R (Recipes (n2:<|bf) e1 bt e2 Empty) = Recipes Empty n2 bf e1 (bt|>e2)
shift2R (Recipes bf e1 bt e2 (n2:<|af)) = Recipes bf e1 (bt|>e2) n2 af

shrinkR :: Recipes -> Recipes
shrinkR (Recipes Empty e1 Empty e2 Empty) = (Recipes Empty e1 Empty e2 Empty)
shrinkR (Recipes (bf:|>y) e1 Empty e2 Empty) = (Recipes bf y Empty e1 Empty)
shrinkR (Recipes bf e1 (bt:|>y) e2 Empty) = (Recipes bf e1 bt y Empty)
shrinkR (Recipes bf e1 bt e2 (af:|>y)) = (Recipes bf e1 bt e2 af)

toSeq :: Recipes -> Seq Char
toSeq (Recipes bf e1 bt e2 af) = (bf|>e1) >< bt >< (e2<|af)

make :: Int -> String
make n = toList $ take 10 $ drop n $ toSeq $ head $ dropWhile ((flip (<) (n+10)) . size) $ iterate move startRecipes

search :: Seq Char -> Int
search s = (fromJust $ index stopAt) + size stopAt - 1 - length s
  where stopAt = head $ dropWhile (not . found) $ iterate move startRecipes
        found r = isJust $ index r
        index r
          | (endWith s r)           = Just 1
          | (endWith s (shrinkR r)) = Just 0
          | otherwise               = Nothing

endWith :: Seq Char -> Recipes -> Bool
endWith Empty _ = True
endWith (xs:|>x) (Recipes Empty e1 Empty e2 Empty) = (x==e2) && (xs == singleton e1)
endWith (xs:|>x) (Recipes (bf:|>y) e1 Empty e2 Empty) = (x==e2) && endWith xs (Recipes bf y Empty e1 Empty)
endWith (xs:|>x) (Recipes bf e1 (bt:|>y) e2 Empty) = (x==e2) && endWith xs (Recipes bf e1 bt y Empty)
endWith (xs:|>x) (Recipes bf e1 bt e2 (af:|>y)) = (x==y) && endWith xs (Recipes bf e1 bt e2 af)
