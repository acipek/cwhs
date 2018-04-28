module Src.Lib.Kyu6.Collatz.Collatz(collatz) where
import Data.List

collatz :: Int -> String
collatz = intercalate "->" . map show . collatzHelper

collatzHelper :: Int -> [Int]
collatzHelper n | n == 1 = [n]
                | even n = n : collatzHelper (n `div` 2)
                | odd n = n : collatzHelper (3*n + 1)
