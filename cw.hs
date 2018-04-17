-- Integers: Recreation One
-- Given two integers m, n (1 <= m <= n) we want to find all integers between m and n whose sum of squared divisors is itself a square. 42 is such a number.

divisors :: Int -> [Int]
divisors x = filter ((==0) . (x `mod`)) [1..x]

squaredDivisors :: [Int] -> Bool
squaredDivisors xs = t == round (sqrt (fromIntegral t)) * round (sqrt (fromIntegral t))
                     where t = sum(map (^2) xs)

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = map (\x -> (last x, sum (map (^2) x))) (filter squaredDivisors (map divisors [m..n]))

