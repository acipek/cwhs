Given two numbers and an arithmetic operator (the name of it, as a string), return the result of the two numbers having that operator used on them.

a and b will both be positive integers, and a will always be the first number in the operation, and b always the second.

The four operators are "add", "subtract", "divide", "multiply".

A few examples:

-- In Haskell:

-- 1. The operation is defined as
data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

-- 2. The arithmetic function as 
arithmetic :: Double -> Double -> Operation -> Double
arithmetic :: Fractional a => a -> a -> Operation -> a

Try to do it without using if statements!

