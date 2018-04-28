module Src.Lib.Kyu6.TortoiseRacing.TortoiseRacing where

race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g
  | v1 >= v2 = Nothing
  | otherwise = let t = g `div` (v2 - v1)
                    (h, m') = t `quotRem` 3600
                    (m, s) = m' `quotRem` 60
                in Just (h,m,s)
