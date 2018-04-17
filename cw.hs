import           Control.Monad
import           Data.List

-- Integers: Recreation One
-- Given two integers m, n (1 <= m <= n) we want to find all integers between m and n whose sum of squared divisors is itself a square. 42 is such a number.
divisors :: Int -> [Int]
divisors x = filter ((== 0) . (x `mod`)) [1 .. x]

squaredDivisors :: [Int] -> Bool
squaredDivisors xs =
  t == round (sqrt (fromIntegral t)) * round (sqrt (fromIntegral t))
  where
    t = sum (map (^ 2) xs)

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n =
  map
    (\x -> (last x, sum (map (^ 2) x)))
    (filter squaredDivisors (map divisors [m .. n]))

-- Tortoise Racing
-- Given two speeds v1 (A's speed, integer > 0) and v2 (B's speed, integer > 0) and a lead g (integer > 0) how long will it take B to catch A?

race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g
  | v1 >= v2 = Nothing
  | otherwise = let t = g `div` (v2 - v1)
                    (h, m') = t `quotRem` 3600
                    (m, s) = m' `quotRem` 60
                in Just (h,m,s)

-- Moves in squared strings (III)
-- You are given a string of n lines, each substring being n characters long: For example:
--
-- s = "abcd\nefgh\nijkl\nmnop"
-- 
-- We will study some transformations of this square of strings.

-- Symmetry with respect to the main diagonal: diag_1_sym (or diag1Sym or diag-1-sym)

--  diag_1_sym(s) => "aeim\nbfjn\ncgko\ndhlp"

-- Clockwise rotation 90 degrees: rot_90_clock (or rot90Clock or rot-90-clock)

--  rot_90_clock(s) => "miea\nnjfb\nokgc\nplhd"

-- selfie_and_diag1(s) (or selfieAndDiag1 or selfie-and-diag1) It is initial string + string obtained by symmetry with respect to the main diagonal.

--  s = "abcd\nefgh\nijkl\nmnop" -->
--    "abcd|aeim\nefgh|bfjn\nijkl|cgko\nmnop|dhlp"

diag1Sym :: [Char] -> [Char]
diag1Sym = intercalate "\n" . transpose . lines

rot90Clock :: [Char] -> [Char]
rot90Clock = intercalate "\n" . map reverse . transpose . lines

selfieAndDiag1 :: [Char] -> [Char]
selfieAndDiag1 strng = intercalate "\n" (zipWith (\a b -> a ++ "|" ++ b) (lines strng) (lines (diag1Sym strng)))

-- Hyper Sphere
-- To pass this kata you are required to complete the function in_sphere?. You will be given an array of cordinates and a radius. The function should return true if the coordinates describe a point within the given radius of the origin ([0,0...0]). A point with no co-ordinates should return true. (In zero dimensions all points are the same point)

inSphere :: (Ord a, Num a) => [a] -> a -> Bool
inSphere = flip ((>=) . (^ 2)) . sum . map (^ 2)
