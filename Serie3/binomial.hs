binom_naiv :: Integer -> Integer -> Integer
binom_naiv n k = product [1..n] `div` (product [1..k] * product [1..x])
  where x = n-k


binom_recurs :: Int -> Int -> Int
binom_recurs n k
    | k == 0 = 1
    | n == k = 1
    | k == 1 = n
    | n == (k - 1) = n
    | otherwise = binom_recurs (n - 1) (k - 1) + binom_recurs (n - 1) k

binom_recurs_efficient :: Integer -> Integer -> Integer
binom_recurs_efficient n k
    | k == 1 = 1
    | otherwise = product[ x .. n] `div` product[1 .. k]
        where x = n - k
