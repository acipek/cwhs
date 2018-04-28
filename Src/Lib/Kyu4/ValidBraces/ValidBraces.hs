module Src.Lib.Kyu4.ValidBraces.ValidBraces(validBraces) where

validBraces :: String -> Bool
validBraces xs = br xs []

br :: String -> String -> Bool
br [] _ = True 
br braces open | (head braces) `elem` "({[" = br (tail braces) (head braces:open)
               | head braces == ')' = if open /= [] && head open == '(' then br (tail braces) (tail open) else False
               | head braces == ']' = if open /= [] && head open == '[' then br (tail braces) (tail open) else False
               | head braces == '}' = if open /= [] && head open == '{' then br (tail braces) (tail open) else False
