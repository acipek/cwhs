module Src.Lib.Kyu5.IntegersRecreationOne.IntegersRecreationOne(listSquared) where

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

