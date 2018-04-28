module Src.Lib.Kyu6.EsolangTick.EsolangTick(interpreter) where
import Data.Char

interpreter :: String -> String
interpreter tape = tick' tape [(0,0)] 0

tick' :: String -> [(Int, Int)] -> Int -> String
tick' [] _ _ = []
tick' (x:xs) selector a | x == '+' = if snd t == 255
                                          then tick' xs (changeSpec selector a 0) a
                                          else tick' xs (changeSpec selector a (snd t + 1)) a
                             | x == '*' = chr (snd t) : tick' xs selector a
                             | x == '>' = tick' xs (selector ++ [(a+1, 0)]) (a+1)
                             | x == '<' = tick' xs selector (a-1)
                             | otherwise = tick' xs selector a
                               where t = if any ((== a) . fst) selector then head (filter ((==a) . fst) selector) else (0,0)

changeSpec :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
changeSpec xs n x = take n xs ++ [(n,x)] ++ drop (n + 1) xs
