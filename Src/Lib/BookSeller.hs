module Src.Lib.BookSeller where

data Stock =
  Stock String
        Int
  deriving (Show, Eq)

ch :: Stock -> String
ch (Stock c _) = c

nb :: Stock -> Int
nb (Stock _ n) = n

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] _            = []
stocklist _ []            = []
stocklist l (x:cs) = [(x, count l x)] ++ stocklist l cs

count :: [Stock] -> Char -> Int
count [] x = 0
count (t:st) x =
  let c = head (ch t)
      n = nb t
  in if c == x
       then n + count st x
       else count st x

stock = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]
