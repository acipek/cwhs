module Src.Lib.Kyu6.MiniStringFuckInterpreter.MiniStringFuckInterpreter where
import Data.Char

myFirstInterpreter :: String -> String
myFirstInterpreter code = f code 0
                          where f :: String -> Int -> String
                                f [] _ = []
                                f (x:xs) n | x == '+' = if n == 255 then f xs 0 else f xs (n+1)
                                           | x == '.' = chr n : f xs n
                                           | otherwise = f xs n
