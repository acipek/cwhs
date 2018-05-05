module Src.Lib.Kyu5.TheHashtagGenerator.TheHashtagGenerator where

import           Data.Char

generateHashtag :: String -> Maybe String
generateHashtag xs =
  if length t > 140 || length t == 0
    then Nothing
    else Just ("#" ++ t)
  where
    t = concatMap (\x -> toUpper (head x) : tail x) (words xs)
