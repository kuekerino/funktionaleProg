mersenne :: Int -> [Int]
mersenne 0 = [0]
mersenne n = (x:xs)
  where x =
    and xs = 2^(n-1)
