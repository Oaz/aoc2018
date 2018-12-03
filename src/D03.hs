module D03
    ( Claim (..), mkClaims, claimsIntersectionSize, isolatedClaim
    ) where

import Data.List
import qualified Data.HashMap.Strict as HM
import Data.Complex
import Text.Regex

data Claim = Claim { identifier :: Int,
                     topLeft :: Complex Int,
                     bottomRight :: Complex Int }
  deriving (Show, Eq)

type World = HM.HashMap (Complex Int) [Claim]

isolatedClaim :: String -> Int
isolatedClaim input = identifier $ head $ claims \\ badClaims
  where
    badClaims = concat $ HM.elems $ HM.filter (\cs -> (length cs) > 1) $ buildWorld claims
    claims = mkClaims input

claimsIntersectionSize :: String -> Int
claimsIntersectionSize = length . filter (> 1) . map length . HM.elems . buildWorld . mkClaims

buildWorld :: [Claim] -> World
buildWorld = foldl addClaimToWorld HM.empty

addClaimToWorld :: World -> Claim -> World
addClaimToWorld world claim = foldl addPoint world [x:+y | x<-[(realPart p1)..(realPart p2)], y<-[(imagPart p1)..(imagPart p2)]]
  where
    p1 = topLeft claim
    p2 = bottomRight claim
    addPoint w p = HM.insertWith (++) p [claim] w

mkClaims :: String -> [Claim]
mkClaims = map mkClaim . lines

mkClaim :: String -> Claim
mkClaim def = Claim (params !! 0) (left :+ top) ((left+width-1) :+ (top+height-1))
  where
    top = params !! 2
    left = params !! 1
    width = params !! 3
    height = params !! 4
    params = extract $ matchRegexAll (mkRegex "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)") def
    extract (Just (_,_,_,subs)) = map read subs


