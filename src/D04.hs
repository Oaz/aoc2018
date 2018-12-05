
module D04
( Sleep(..), mostFrequentlyAsleepGuardMinute, mostSleepingGuardMinute, gatherSleeps, emptyLog, parseLog, beginsShift, fallsAsleep, wakesUp, logTime
) where

import Data.List
import Text.Parsec
import Data.Time
import Data.Functor.Identity

data Sleep = Sleep { guard :: Int, day :: Day, minute :: Int } deriving (Show, Eq)
data LogCursor = LogCursor { currentGuard :: Int, currentTime :: LocalTime, sleepStart :: Int, sleeps :: [Sleep] }
type LogParser a = ParsecT String LogCursor Identity a

mostFrequentlyAsleepGuardMinute :: [Sleep] -> (Int,Int)
mostFrequentlyAsleepGuardMinute ss = fst $ head $ filter (\(gm,c) -> (c == highestMinuteCount)) guardMinutes
  where
    highestMinuteCount = maximum $ map snd guardMinutes
    guardMinutes = occurrences $ map (\s -> (guard s, minute s)) ss

mostSleepingGuardMinute :: [Sleep] -> (Int,Int)
mostSleepingGuardMinute ss = (mostSleepingGuard,mostSleepingMinute)
  where
    mostSleepingMinute = fst $ highestOccurrence $ map minute sleepings
    sleepings = filter (\s -> ((guard s)==mostSleepingGuard)) ss
    mostSleepingGuard = fst $ highestOccurrence $ map guard ss

highestOccurrence :: (Ord a,Eq a) => [a] -> (a,Int)
highestOccurrence xs = head $ filter (\(item,size) -> (size == highest)) counts
  where
    highest = maximum $ map snd counts
    counts = occurrences xs

occurrences :: (Ord a,Eq a) => [a] -> [(a,Int)]
occurrences = map (\x -> (head x, length x)) . group . sort

gatherSleeps :: String -> [Sleep]
gatherSleeps s = ss
  where (Right ss) = runParser parseLog emptyLog "" (unlines $ sort $ lines s)

emptyLog :: LogCursor
emptyLog = LogCursor 0 (LocalTime (ModifiedJulianDay 0) midnight) 0 []

parseLog :: LogParser [Sleep]
parseLog = do { many logRow ; s <- getState ; return $ sleeps s }
  where logRow = do { logTime; choice [beginsShift, fallsAsleep, wakesUp] } 

beginsShift :: LogParser Int
beginsShift = do { try (string " Guard #")
             ; x <- many1 digit
             ; string " begins shift\n"
             ; let guardID = read x
             ; modifyState (\s -> s { currentGuard = read x })
             ; return guardID
             }

fallsAsleep :: LogParser Int
fallsAsleep = do { try (string " falls asleep\n") ; modifyState (\s -> s { sleepStart = todMin $ localTimeOfDay $ currentTime s }) ; return 0 }

wakesUp :: LogParser Int
wakesUp = do { try (string " wakes up\n") ; modifyState addLatestSleeps ; return 0 }

addLatestSleeps :: LogCursor -> LogCursor
addLatestSleeps s = s { sleeps = (sleeps s) ++ latestSleeps }
    where
      latestSleeps = [Sleep (currentGuard s) (localDay now) m | m <- sleepingMinutes]
      sleepingMinutes = [(sleepStart s)..((todMin $ localTimeOfDay now)-1)]
      now = currentTime s
              
logTime :: LogParser LocalTime
logTime = do { ts <- logAsString
             ; let t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" ts
             ; modifyState (\s -> s { currentTime = t })
             ; return t
             }
  where logAsString = do { char '[' ; dt <- many (noneOf "]") ; char ']' ; return dt }
