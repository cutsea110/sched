module Sched (sched) where

import Data.Time (Day)
import Types (Item(..), Unit, Schedule)

workOn :: Unit      -- ^ Maximum unit number
       -> Int       -- ^ Repetition number
       -> Int       -- ^ Day number
       -> [[Unit]]  -- ^ List of cycles with units to work on that day
workOn unitMax repetition n = map (filter valid . unitsAt) [1..repetition]
  where valid :: Int -> Bool
        valid m = 0 < m && m <= unitMax
        unitsAt :: Int -> [Unit]
        unitsAt i | 1 <= i && i <= 7  = let u = n - diff1 !! (i-1)  in [u]
                  | 8 <= i && i <= 10 = let u = n - diff2 !! (i-8)  in if odd u then [u, u+1] else []
                  | 11 <= i           = let u = n - diff4 !! (i-11) in if by4 u then [u-3, u-2, u-1, u] else []
                  | otherwise         = error "unexpected index in rule"
          where
            by4 :: Int -> Bool
            by4 x = x `mod` 4 == 0

            diff1 = [0,1,4,9,17,29,46]
            diff2 = [67,88,109]
            diff4 = 129:[155,182..]

sched :: Unit      -- ^ Maximum unit number
      -> Int       -- ^ Number of repetitions
      -> Day       -- ^ Starting day
      -> Int       -- ^ Number of days to schedule
      -> Schedule  -- ^ Generated schedule
sched unitMax repetition start n = zipWith Item days $ map work [1..]
  where days = take n [start..]
        work = workOn unitMax repetition
