module Types (Unit, Cycle, DayItem(..), Schedule) where

import Data.Time (Day)

type Unit     = Int
type Cycle    = Int
data DayItem  = Item { date :: Day, units :: [[Unit]] }
type Schedule = [DayItem]
