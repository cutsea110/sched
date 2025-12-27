module Main where

import Data.Time
import Data.Time.Format ()

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"
  
today :: IO Day
today = utctDay <$> getCurrentTime

type Unit = Int
type Cycle = Int
type Schedule = [(Day, [[Unit]])]


dayN'sWork :: Unit      -- ^ Maximum unit number
           -> Int       -- ^ Day number
           -> [[Unit]]  -- ^ List of cycles with units to work on that day
dayN'sWork unitMax n = map (filter p) $ map rule [1..18]
  where p :: Int -> Bool
        p m = 0 < m && m <= unitMax
        rule :: Int -> [Unit]
        rule i | 1 <= i && i <= 7  =
                  let a = diff1 !! (i-1)
                  in [n-a]
                | 8 <= i && i <= 10 =
                  let a = diff2 !! (i-8)
                  in if odd (n-a) then [n-a, n-a+1] else []
                | 11 <= i           =
                  let a = diff4 !! (i-11)
                  in if quad (n-a) then [n-a-3,n-a-2,n-a-1, n-a] else []
                | otherwise         = error "unexpected index in rule"
          where
            quad :: Int -> Bool
            quad x = x `mod` 4 == 0

            diff1 = [0,1,4,9,17,29,46]
            diff2 = [67,88,109]
            diff4 = 129:[155,182..]
               


daysFromDay :: Day    -- ^ Starting day
            -> Int    -- ^ Number of days to generate
            -> [Day]  -- ^ List of days
daysFromDay start n = take n $ iterate (addDays 1) start


sched :: Unit      -- ^ Maximum unit number
      -> Day       -- ^ Starting day
      -> Int       -- ^ Number of days to schedule
      -> Schedule  -- ^ Generated schedule
sched unitMax start d = zip days (map (dayN'sWork unitMax) [1..])
  where days = daysFromDay start d

sched' :: Unit     -- ^ Maximum unit number
       -> String   -- ^ Starting date as string
       -> Int      -- ^ Number of days to schedule
       -> Schedule -- ^ Generated schedule
sched' unitMax dateStr d = case parseDay dateStr of
  Just start -> sched unitMax start d
  Nothing    -> error $ "Invalid date format: " ++ dateStr

lookupWork :: Schedule       -- ^ Schedule to look up
           -> String         -- ^ Date as string
           -> Maybe [[Unit]] -- ^ Work scheduled for that date
lookupWork m dateStr = case parseDay dateStr of
  Just day -> lookup day m
  Nothing  -> error $ "Invalid date format: " ++ dateStr

main :: IO ()
main = do
  let day = case parseDay "2025-04-01" of
              Just d  -> d
              Nothing -> error "Invalid date format"
  let days = daysFromDay day 330
  let m = zip days (map (dayN'sWork 232) [1..])
  print m
