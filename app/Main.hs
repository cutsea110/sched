module Main where

import Data.Time

today :: IO Day
today = utctDay <$> getCurrentTime


main :: IO ()
main = do
  day <- today
  putStrLn $ "Today's date is: " ++ show day
