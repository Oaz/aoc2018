module D24Spec (spec) where

import Test.Hspec
import D24
import Data.List

shorten = map short where short (GIA id1 id2 (Group units _)) = (id1, id2, units)

spec :: Spec
spec = do

  describe "building an army" $ do
    it "attack type 1" $ make anAttackType "slashing" `shouldBe` Right Slashing
    it "attack type 2" $ make anAttackType "radiation" `shouldBe` Right Radiation

    it "powers" $
      make somePowers "each with 2584 hit points with an attack that does 81 bludgeoning damage at initiative 18"
        `shouldBe` Right (Powers 2584 Bludgeoning 81 18 [] [])

    it "attack types" $
      make aListOfAttackTypes "weak to fire, radiation, bludgeoning"
        `shouldBe` Right (Weaken,[Fire,Radiation,Bludgeoning])

    it "powers with weaknesses" $
      make somePowers "each with 6315 hit points (weak to slashing) with an attack that does 37 slashing damage at initiative 14"
        `shouldBe` Right (Powers 6315 Slashing 37 14 [Slashing] [])

    it "powers with immunities" $
      make somePowers "each with 6315 hit points (immune to radiation, bludgeoning) with an attack that does 37 slashing damage at initiative 14"
        `shouldBe` Right (Powers 6315 Slashing 37 14 [] [Radiation,Bludgeoning])
              
    it "powers with weaknesses and immunities" $
      make somePowers "each with 6792 hit points (weak to fire, radiation; immune to cold) with an attack that does 29 bludgeoning damage at initiative 9"
        `shouldBe` Right (Powers 6792 Bludgeoning 29 9 [Fire,Radiation] [Cold])

    it "group" $
      make aGroup "3490 units each with 2584 hit points with an attack that does 81 bludgeoning damage at initiative 18"
        `shouldBe` Right (Group 3490 (Powers 2584 Bludgeoning 81 18 [] []))

    it "army" $
      make anArmy "Immune System:\n\
                  \3490 units each with 2584 hit points with an attack that does 81 bludgeoning damage at initiative 18\n\
                  \23 units each with 6315 hit points (immune to radiation, bludgeoning) with an attack that does 37 slashing damage at initiative 14\n\
                  \145 units each with 6792 hit points (weak to fire, radiation; immune to cold) with an attack that does 29 bludgeoning damage at initiative 9\n"
        `shouldBe` Right (Army "Immune System" [ Group 3490 (Powers 2584 Bludgeoning 81 18 [] [])
                                               , Group 23 (Powers 6315 Slashing 37 14 [] [Radiation,Bludgeoning])
                                               , Group 145 (Powers 6792 Bludgeoning 29 9 [Fire,Radiation] [Cold])
                                               ])
 
    it "part 1" $ do  { input <- readFile "test/D24.txt"
                      ; let Right armies = make aFight input
                      ; length armies `shouldBe` 2
                      ; name (armies !! 0) `shouldBe` "Immune System"
                      ; name (armies !! 1) `shouldBe` "Infection"
                      }

  describe "add IDs" $ do
    it "level 1" $ withID ["foo","bar","qix"] `shouldBe` [(1,"foo"),(2,"bar"),(3,"qix")]

    it "level 2" $ withID2 id ["abc","ij","wxyz"] `shouldBe` [ (1,1,'a'), (1,2,'b'), (1,3,'c')
                                                             , (2,4,'i'), (2,5,'j')
                                                             , (3,6,'w'), (3,7,'x'), (3,8,'y'), (3,9,'z')]  
 
  describe "ord composition" $ do
    it "comparing2" $ sortBy (comparing2 length head) ["foo", "fizz", "bar", "qix", "buzz"]
                                           `shouldBe` ["bar", "foo", "qix", "buzz", "fizz"]

    it "comparing3" $ sortBy (comparing3 length head last) ["fizy", "foo", "fizz", "baz", "bar", "qix", "buzz"]
                                                `shouldBe` ["bar", "baz", "foo", "qix", "buzz", "fizy", "fizz"]
  
  describe "sample" $ do

    let Right armies = make aFight
                        "Immune System:\n\
                        \17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n\
                        \989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\
                        \\n\
                        \Infection:\n\
                        \801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n\
                        \4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4\n"

    let battlefield1 = mkBattlefield armies
    it "battlefield" $ shorten battlefield1 `shouldBe` [(1,1,17),(1,2,989),(2,3,801),(2,4,4485)]

    let fight1@(F _ selection1) = targetSelection battlefield1
    it "selection for 1st fight" $ selection1 `shouldBe` [A 2 3 3, A 4 2 4, A 1 4 2, A 3 1 1]

    let battlefield2 = resolveAttack fight1
    it "battlefield after 1st fight" $ shorten battlefield2 `shouldBe` [(1,2,905),(2,3,797),(2,4,4434)]

    let fight2@(F _ selection2) = targetSelection battlefield2
    it "selection for 2nd fight" $ selection2 `shouldBe` [A 2 3 3, A 3 2 1]

    let battlefield3 = resolveAttack fight2
    it "battlefield after 2nd fight" $ shorten battlefield3 `shouldBe` [(1,2,761),(2,3,793),(2,4,4434)]

    let battlefield4 = resolveAttack $ targetSelection battlefield3
    it "battlefield after 3rd fight" $ shorten battlefield4 `shouldBe` [(1,2,618),(2,3,789),(2,4,4434)]

    let battlefield5 = resolveAttack $ targetSelection battlefield4
    it "battlefield after 4th fight" $ shorten battlefield5 `shouldBe` [(1,2,475),(2,3,786),(2,4,4434)]

    let battlefield6 = resolveAttack $ targetSelection battlefield5
    it "battlefield after 5th fight" $ shorten battlefield6 `shouldBe` [(1,2,333),(2,3,784),(2,4,4434)]

    let battlefield7 = resolveAttack $ targetSelection battlefield6
    it "battlefield after 6th fight" $ shorten battlefield7 `shouldBe` [(1,2,191),(2,3,783),(2,4,4434)]

    let battlefield8 = resolveAttack $ targetSelection battlefield7
    it "battlefield after 7th fight" $ shorten battlefield8 `shouldBe` [(1,2,49),(2,3,782),(2,4,4434)]

    let battlefield9 = resolveAttack $ targetSelection battlefield8
    it "battlefield after 8th fight" $ shorten battlefield9 `shouldBe` [(2,3,782),(2,4,4434)]

    it "unroll war" $ unrollWar armies `shouldBe` 5216

    it "find minimal boost" $ findMinimalBoost armies `shouldBe` 1570
  
  describe "part1" $ do

    it "unroll war" $ do { input <- readFile "test/D24.txt"
                         ; let (Right armies) = make aFight input
                         ; unrollWar armies `shouldBe` 10890
                         }
  
  describe "part2" $ do

    it "find minimal boost" $ do  { input <- readFile "test/D24.txt"
                                  ; let (Right armies) = make aFight input
                                  ; findMinimalBoost armies `shouldBe` 61
                                  ; unrollWar (boost [61,0] armies) `shouldBe` 7730
                                  }
                      