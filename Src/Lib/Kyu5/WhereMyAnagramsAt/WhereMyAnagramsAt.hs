module Src.Lib.Kyu5.WhereMyAnagramsAt.WhereMyAnagramsAt(anagrams) where

import           Data.List

anagrams :: String -> [String] -> [String]
anagrams w ws = map (ws !!) (elemIndices (counts w) (map counts ws))

counts :: String -> [(Char, Int)]
counts s = map (\x -> (head x, length x)) (group (sort s))
