module Src.Lib.Kyu4.PascalsTriangle.PascalsTriangle where

pascalsTriangle :: Int -> [Int]
pascalsTriangle 1 = [1]
pascalsTriangle n = pascalsTriangle (n-1) ++ pascalsTriangleLevel n

sums :: [Int] -> [Int]
sums [x] = [x]
sums [x,y] = [x+y]
sums (x:y:xs) = (x+y) : sums (y:xs)

pascalsTriangleLevel :: Int -> [Int]
pascalsTriangleLevel 1 = [1]
pascalsTriangleLevel 2 = [1,1]
pascalsTriangleLevel n = [1] ++ sums (pascalsTriangleLevel (n-1)) ++ [1]
