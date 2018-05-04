module Src.Lib.Kyu5.PalindromeChainLength.PalindromeChainLength(palindromeChainLength)
where
import Data.Char

palindromeChainLength :: Integer -> Integer
palindromeChainLength n | isPalindrome (show n) = 0
                        | otherwise = 1 + palindromeChainLength (n + (read . reverse . show) n)


isPalindrome :: String -> Bool
isPalindrome x | length x <= 1 = True
               | head x == last x = isPalindrome (tail (init x))
               | otherwise = False 
