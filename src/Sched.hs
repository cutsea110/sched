module Sched (sched) where

import Data.Time (Day)
import Types (Unit, Schedule)

workOn :: Unit      -- ^ Maximum unit number
       -> Int       -- ^ Repetition number
       -> Int       -- ^ Day number
       -> [[Unit]]  -- ^ List of cycles with units to work on that day
workOn unitMax repetition n = map (filter p) $ map rule [1..repetition]
  where p :: Int -> Bool
        p m = 0 < m && m <= unitMax
        rule :: Int -> [Unit]
        rule i | 1 <= i && i <= 7 =
                 let a = diff1 !! (i-1)
                 in [n-a]
               | 8 <= i && i <= 10 =
                 let a  = diff2 !! (i-8)
                     n' = n-a
                 in if odd n' then [n', n'+1] else []
               | 11 <= i =
                 let a  = diff4 !! (i-11)
                     n' = n-a
                 in if quad n' then [n'-3, n'-2, n'-1, n'] else []
               | otherwise = error "unexpected index in rule"
          where
            quad :: Int -> Bool
            quad x = x `mod` 4 == 0

            diff1 = [0,1,4,9,17,29,46]
            diff2 = [67,88,109]
            diff4 = 129:[155,182..]

sched :: Unit      -- ^ Maximum unit number
      -> Int       -- ^ Number of repetitions
      -> Day       -- ^ Starting day
      -> Int       -- ^ Number of days to schedule
      -> Schedule  -- ^ Generated schedule
sched unitMax repetition start n = zip days $ map work [1..]
  where days = take n [start..]
        work = workOn unitMax repetition
