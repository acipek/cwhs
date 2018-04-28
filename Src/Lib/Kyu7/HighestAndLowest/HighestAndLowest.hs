module Src.Lib.Kyu7.HighestAndLowest.HighestAndLowest(highAndLow) where
import Data.List

highAndLow :: String -> String
highAndLow input = show (last (sortStrList input)) ++ " " ++ show (head (sortStrList input)) 

sortStrList :: String -> [Int]
sortStrList xs = sort (map (\x -> read x :: Int) (words xs))

