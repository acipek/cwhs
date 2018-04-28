module Src.Lib.Kyu6.BuildTower.BuildTower(buildTower) where

buildTower :: Int -> [String]
buildTower n = buildT n 1 

buildT :: Int -> Int -> [String]
buildT n x | n <= 0 = []
           | otherwise = (take (n - 1) (cycle " ") ++ take x (cycle "*") ++ take (n - 1) (cycle " ")) : buildT (n - 1) (x + 2)
