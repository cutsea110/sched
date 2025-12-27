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
dayN'sWork unitMax n = map (filter pred) $ cycle1:cycle2:cycle3:cycle4:cycle5:cycle6:cycle7:cycle8:cycle9:cycle10:cycle11:cycle12:cycle13:cycle14:cycle15:cycle16:cycle17:cycle18:[]
  where pred m = 0 < m && m <= unitMax
        cycle1  = [n]
        cycle2  = [n-1]
        cycle3  = [n-4]
        cycle4  = [n-9]
        cycle5  = [n-17]
        cycle6  = [n-29]
        cycle7  = [n-46]
        cycle8  = if odd (n-67) then [n-67,n-66] else []
        cycle9  = if odd (n-88) then [n-88,n-87] else []
        cycle10 = if odd (n-109) then [n-109,n-108] else []
        cycle11 = if quad (n-129) then [n-132,n-131,n-130,n-129] else []
        cycle12 = if quad (n-155) then [n-158,n-157,n-156,n-155] else []
        cycle13 = if quad (n-182) then [n-185,n-184,n-183,n-182] else []
        cycle14 = if quad (n-209) then [n-212,n-211,n-210,n-209] else []
        cycle15 = if quad (n-236) then [n-239,n-238,n-237,n-236] else []
        cycle16 = if quad (n-263) then [n-266,n-265,n-264,n-263] else []
        cycle17 = if quad (n-290) then [n-293,n-292,n-291,n-290] else []
        cycle18 = if quad (n-317) then [n-320,n-319,n-318,n-317] else []


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
