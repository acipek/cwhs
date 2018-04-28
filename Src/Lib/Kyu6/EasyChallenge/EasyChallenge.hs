-- Begin your day with a challenge, but an easy one.
module Src.Lib.Kyu6.EasyChallenge.EasyChallenge(oneTwoThree) where

oneTwoThree :: Integer -> [Integer]
oneTwoThree 0 = [0,0]
oneTwoThree n = toInt (calc n) : [toInt (replicate (fromInteger n) 1) :: Integer]

calc :: (Num t, Ord t) => t -> [t]
calc n | n < 10 = [n]
       | otherwise = 9 : calc (n-9)
 
toInt :: (Show a, Foldable t) => t a -> Integer       
toInt xs = (read . concatMap show) xs :: Integer
