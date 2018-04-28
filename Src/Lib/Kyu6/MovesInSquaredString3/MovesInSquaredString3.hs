module Src.Lib.Kyu6.MovesInSquaredString3.MovesInSquaredString3 where
import Data.List

diag1Sym :: String -> String
diag1Sym = intercalate "\n" . transpose . lines

rot90Clock :: String -> String
rot90Clock = intercalate "\n" . map reverse . transpose . lines

selfieAndDiag1 :: String -> String
selfieAndDiag1 strng = intercalate "\n" (zipWith (\a b -> a ++ "|" ++ b) (lines strng) (lines (diag1Sym strng)))

