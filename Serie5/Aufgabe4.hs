minNatNotIn :: [Int] -> Int
minNatNotIn [] = 0
minNatNotIn x
    | 0 `notElem` x = 0
    | otherwise = helper x [1..(maximum x)]

helper :: [Int] -> [Int] -> Int
helper ursprung (x:xs)
    | x `notElem` ursprung = x
    | otherwise = helper ursprung xs
