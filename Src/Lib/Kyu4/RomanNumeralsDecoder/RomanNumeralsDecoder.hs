module Src.Lib.Kyu4.RomanNumeralsDecoder.RomanNumeralsDecoder
  ( romanToInt
  ) where

romans :: [(Char, Int)]
romans =
  [ ('I', 1)
  , ('V', 5)
  , ('X', 10)
  , ('L', 50)
  , ('C', 100)
  , ('D', 500)
  , ('M', 1000)
  ]

getInteger :: Char -> Int
getInteger x = snd (head (filter ((== x) . fst) romans))

helper :: [Int] -> Int -> Int
helper [] total = total
helper [x] total = helper [] (total + x)
helper (x:y:xs) total
  | x >= y = helper (y : xs) (total + x)
  | x < y = helper xs (total + y - x)

romanToInt :: String -> Int
romanToInt roman = helper (map getInteger roman) 0
