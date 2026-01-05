module Types (Unit, Cycle, Item(..), Schedule) where

import Data.Time (Day)

type Unit     = Int
type Cycle    = Int
data Item     = Item { date :: Day, units :: [[Unit]] }
type Schedule = [Item]
