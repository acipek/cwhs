module Src.Lib.Kyu6.FoldAnArray.FoldAnArray(foldList) where

f :: [Int] -> [Int]
f xs = let t = length xs
           y = if odd t then take ((t `div` 2) + 1) xs ++ [0] ++ drop ((t `div` 2) + 1) xs else xs
       in zipWith (+) y (reverse (drop (length y `div`  2) y))

foldList :: [Int] -> Int -> [Int]
foldList xs 0 = xs
foldList xs n = foldList (f xs) (n-1)
