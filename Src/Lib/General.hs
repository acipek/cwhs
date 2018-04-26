
module           Src.Lib.General where
import           Data.List
import           Data.Char

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

diag1Sym :: String -> String
diag1Sym = intercalate "\n" . transpose . lines

rot90Clock :: String -> String
rot90Clock = intercalate "\n" . map reverse . transpose . lines

selfieAndDiag1 :: String -> String
selfieAndDiag1 strng = intercalate "\n" (zipWith (\a b -> a ++ "|" ++ b) (lines strng) (lines (diag1Sym strng)))

-- Hyper Sphere
-- To pass this kata you are required to complete the function in_sphere?. You will be given an array of cordinates and a radius. The function should return true if the coordinates describe a point within the given radius of the origin ([0,0...0]). A point with no co-ordinates should return true. (In zero dimensions all points are the same point)

inSphere :: (Ord a, Num a) => [a] -> a -> Bool
inSphere = flip ((>=) . (^ 2)) . sum . map (^ 2)

--MiniStringFuck interpreter

myFirstInterpreter :: String -> String
myFirstInterpreter code = f code 0
                          where f :: String -> Int -> String
                                f [] _ = []
                                f (x:xs) n | x == '+' = if n == 255 then f xs 0 else f xs (n+1)
                                           | x == '.' = chr n : f xs n
                                           | otherwise = f xs n

--Esolang: Tick interpreter
interpreter :: String -> String
interpreter tape = tick' tape [(0,0)] 0

tick' :: String -> [(Int, Int)] -> Int -> String
tick' [] _ _ = []
tick' (x:xs) selector a | x == '+' = if snd t == 255
                                          then tick' xs (changeSpec selector a 0) a
                                          else tick' xs (changeSpec selector a (snd t + 1)) a
                             | x == '*' = chr (snd t) : tick' xs selector a
                             | x == '>' = tick' xs (selector ++ [(a+1, 0)]) (a+1)
                             | x == '<' = tick' xs selector (a-1)
                             | otherwise = tick' xs selector a
                               where t = if any ((== a) . fst) selector then head (filter ((==a) . fst) selector) else (0,0)

changeSpec :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
changeSpec xs n x = take n xs ++ [(n,x)] ++ drop (n + 1) xs

alphabet = "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+."
hello = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
hello2 = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++**>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>++++++++++++++++++++++++++++++++*>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*<<*>>>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*<<<<*>>>>>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>+++++++++++++++++++++++++++++++++*"

-- Common Denominators
-- convertFracs [(1, 2), (1, 3), (1, 4)] `shouldBe` [(6, 12), (4, 12), (3, 12)]
type Ratio a = (a, a) -- Data.Ratio not suitable for this kata

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs [] = []
convertFracs list = let den = unzip list
                        l = llcm (snd den)
                    in map (\x -> (fst x * l `div` snd x, l)) list

llcm :: Integral a => [a] -> a
llcm = foldr lcm 1 

-- Build Tower
-- number of floors (integer and always greater than 0).
-- Tower block is represented as *

buildTower :: Int -> [String]
buildTower n = buildT n 1 

buildT :: Int -> Int -> [String]
buildT n x | n <= 0 = []
           | otherwise = (take (n - 1) (cycle " ") ++ take x (cycle "*") ++ take (n - 1) (cycle " ")) : buildT (n - 1) (x + 2)

-- Highest and Lowest
-- In this little assignment you are given a string of space separated numbers, and have to return the highest and lowest number.

highAndLow :: String -> String
highAndLow input = show (last (sortStrList input)) ++ " " ++ show (head (sortStrList input)) 

sortStrList :: String -> [Int]
sortStrList xs = sort (map (\x -> read x :: Int) (words xs))

-- Make a function that does arithmetic!
-- Given two numbers and an arithmetic operator (the name of it, as a string), return the result of the two numbers having that operator used on them.

data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

arithmetic :: Fractional a => a -> a -> Operation -> a
arithmetic a b Add = a + b
arithmetic a b Divide = a / b
arithmetic a b Multiply = a * b
arithmetic a b Subtract = a - b

-- Reverse or Rotate
-- The input is a string str of digits. Cut the string into chunks (a chunk here is a substring of the initial string) of size sz (ignore the last chunk if its size is less than sz).

-- If a chunk represents an integer such as the sum of the cubes of its digits is divisible by 2, reverse that chunk; otherwise rotate it to the left by one position. Put together these modified chunks and return the result as a string.

revRot :: String -> Int -> String
revRot strng sz | sz <= 0 = ""
                | strng == "" = ""
                | sz > length strng = ""
                | otherwise = helper (take sz strng) ++ revRot (drop sz strng) sz


helper :: String -> String
helper x = if foldr ((+) . (^ 3) . digitToInt) 0 x `mod` 2 == 0
           then reverse x
           else drop 1 x ++ [head x]

-- Fold an array
-- http://www.codewars.com/kata/fold-an-array

f :: [Int] -> [Int]
f xs = let t = length xs
           y = if odd t then take ((t `div` 2) + 1) xs ++ [0] ++ drop ((t `div` 2) + 1) xs else xs
       in zipWith (+) y (reverse (drop (length y `div`  2) y))

foldList :: [Int] -> Int -> [Int]
foldList xs 0 = xs
foldList xs n = foldList (f xs) (n-1)

-- Begin your day with a challenge, but an easy one.

oneTwoThree :: Integer -> [Integer]
oneTwoThree 0 = [0,0]
oneTwoThree n = toInt (calc n) : [toInt (replicate (fromInteger n) 1) :: Integer]

calc :: (Num t, Ord t) => t -> [t]
calc n | n < 10 = [n]
       | otherwise = 9 : calc (n-9)
 
toInt :: (Show a, Foldable t) => t a -> Integer       
toInt xs = (read . concatMap show) xs :: Integer

-- Roman Numerals Encoder
-- Create a function taking a positive integer as its parameter and returning a string containing the Roman Numeral representation of that integer.

romans :: [(Integer, String)]
romans = [(1, "I"), (4, "IV"), (5, "V"), (9, "IX"), (10, "X"), (40, "XL"), (50, "L"), (90, "XC"), (100, "C"), (400, "CD"), (500, "D"), (900, "CM"), (1000, "M")]

solution :: Integer -> String
solution 0 = []
solution n | n `elem` map fst romans = snd (head (filter ((==n) . fst) romans))
           | otherwise = snd (toRoman n) ++ solution (n - fst (toRoman n))

toRoman :: Integer -> (Integer, String)           
toRoman n = romans !! last (findIndices (<n) (map fst romans))

-- Checkerboard Generation
-- Write a method checkerboard/Checkerboard which takes an integer size and returns a checkerboard of dimensions size x size. There are two colored squares on the checkerboard. Red squares are represented by the string [r] and black squares are represented by the string [b]. You MUST use the newline character \n to insert a newline at the end of each row.

checkerboard :: Int -> String
checkerboard n | n <= 0 = ""
               | n == 1 = "[r]\n"
               | even n = concat (replicate (n `div` 2) (concat (replicate (n `div` 2) "[r][b]")++"\n" ++ (concat (replicate (n `div` 2) "[b][r]")++"\n")))
               | odd n = concat (replicate (n `div` 2) (concat (replicate (n `div` 2) "[r][b]")++"[r]\n" ++ (concat (replicate (n `div` 2) "[b][r]")++"[b]\n")) ++ [(concat (replicate (n `div` 2) "[r][b]") ++ "[r]\n")])


-- Collatz
-- A collatz sequence, starting with a positive integern, is found by repeatedly applying the following function to n until n == 1 :
-- n = { n / 2 for even n ;
--      3n + 1 for odd n }
--
-- Create a function collatz that returns a collatz sequence string starting with the positive integer argument passed into the function, in the following form: "X0->X1->...->XN"

collatz :: Int -> String
collatz = intercalate "->" . map show . collatzHelper

collatzHelper :: Int -> [Int]
collatzHelper n | n == 1 = [n]
                | even n = n : collatzHelper (n `div` 2)
                | odd n = n : collatzHelper (3*n + 1)
