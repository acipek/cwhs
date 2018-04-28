module Src.Lib.Kyu6.ReverseOrRotate.ReverseOrRotate(revRot) where
import Data.Char

revRot :: String -> Int -> String
revRot strng sz | sz <= 0 = ""
                | strng == "" = ""
                | sz > length strng = ""
                | otherwise = helper (take sz strng) ++ revRot (drop sz strng) sz


helper :: String -> String
helper x = if foldr ((+) . (^ 3) . digitToInt) 0 x `mod` 2 == 0
           then reverse x
           else drop 1 x ++ [head x]
