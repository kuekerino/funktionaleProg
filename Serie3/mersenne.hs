mersenne :: Int -> [Int]
mersenne n
    | n == 0 = [0] -- Rekursionsanker
    | otherwise =  mersenne (n-1) ++ [(2^n) -1 ]
    -- rekursive Definition der Funktion
