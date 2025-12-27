module Main where

import Data.Time
import Data.Time.Format (defaultTimeLocale)

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
  
today :: IO Day
today = utctDay <$> getCurrentTime

type Unit = Int
type Cycle = Int
type Schedule = [(Day, [[Unit]])]

quad :: Int -> Bool
quad n = n `mod` 4 == 0


dayN'sWork :: Unit -> Int -> [[Unit]]
dayN'sWork unitMax n = map (filter pred) $ map cycle [1..18]
  where pred m = 0 < m && m <= unitMax
        cycle i | 1 <= i && i <= 7  = let a = diff1 !! (i-1) in [n-a]
                | 8 <= i && i <= 10 = let a = diff2 !! (i-8) in if odd (n-a) then [n-a, n-a+1] else []
                | 11 <= i           = let a = diff4 !! (i-11) in if quad (n-a) then [n-a-3,n-a-2,n-a-1, n-a] else []
        diff1 = [0,1,4,9,17,29,46]
        diff2 = [67,88,109]
        diff4 = 129:[155,182..]
               


daysFromDay :: Day -> Int -> [Day]
daysFromDay day n = take n $ iterate (addDays 1) day


sched :: Unit -> Day -> Int -> Schedule
sched unitMax start d = zip days (map (dayN'sWork unitMax) [1..])
  where days = daysFromDay start d

sched' :: Unit -> String -> Int -> Schedule
sched' unitMax dateStr d = case parseDay dateStr of
  Just start -> sched unitMax start d
  Nothing    -> error $ "Invalid date format: " ++ dateStr

lookupWork :: Schedule -> String -> Maybe [[Unit]]
lookupWork sched dateStr = case parseDay dateStr of
  Just day -> lookup day sched
  Nothing  -> error $ "Invalid date format: " ++ dateStr

main :: IO ()
main = do
  let Just day = parseDay "2025-04-01"
  let days = daysFromDay day 330
  let m = zip days (map (dayN'sWork 232) [1..])
  print m
