module Src.Lib.Kyu6.HyperSphere.HyperSphere where

inSphere :: (Ord a, Num a) => [a] -> a -> Bool
inSphere = flip ((>=) . (^ 2)) . sum . map (^ 2)

