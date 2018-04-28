module Src.Lib.Kyu5.CommonDenominators.CommonDenominators(convertFracs) where

type Ratio a = (a, a) -- Data.Ratio not suitable for this kata

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs [] = []
convertFracs list = let den = unzip list
                        l = llcm (snd den)
                    in map (\x -> (fst x * l `div` snd x, l)) list

llcm :: Integral a => [a] -> a
llcm = foldr lcm 1 
