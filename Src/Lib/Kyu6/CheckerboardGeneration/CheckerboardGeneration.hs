module Src.Lib.Kyu6.CheckerboardGeneration.CheckerboardGeneration where

checkerboard :: Int -> String
checkerboard n | n <= 0 = ""
               | n == 1 = "[r]\n"
               | even n = concat (replicate (n `div` 2) (concat (replicate (n `div` 2) "[r][b]")++"\n" ++ (concat (replicate (n `div` 2) "[b][r]")++"\n")))
               | odd n = concat (replicate (n `div` 2) (concat (replicate (n `div` 2) "[r][b]")++"[r]\n" ++ (concat (replicate (n `div` 2) "[b][r]")++"[b]\n")) ++ [(concat (replicate (n `div` 2) "[r][b]") ++ "[r]\n")])
