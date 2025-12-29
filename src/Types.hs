module Types (Unit, Cycle, DayItem, Schedule) where

import Data.Time (Day)

type Unit     = Int
type Cycle    = Int
type DayItem  = (Day, [[Unit]])
type Schedule = [DayItem]
