mersenne :: Int -> [Int]
mersenne n
    | n == 0 = [0]
    | otherwise =  mersenne (n-1) ++ [(2^n) -1 ]
