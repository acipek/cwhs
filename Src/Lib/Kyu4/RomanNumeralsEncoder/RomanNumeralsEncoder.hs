module Src.Lib.Kyu4.RomanNumeralsEncoder.RomanNumeralsEncoder(solution) where
import Data.List

romans :: [(Integer, String)]
romans = [(1, "I"), (4, "IV"), (5, "V"), (9, "IX"), (10, "X"), (40, "XL"), (50, "L"), (90, "XC"), (100, "C"), (400, "CD"), (500, "D"), (900, "CM"), (1000, "M")]

solution :: Integer -> String
solution 0 = []
solution n | n `elem` map fst romans = snd (head (filter ((==n) . fst) romans))
           | otherwise = snd (toRoman n) ++ solution (n - fst (toRoman n))

toRoman :: Integer -> (Integer, String)           
toRoman n = romans !! last (findIndices (<n) (map fst romans))
