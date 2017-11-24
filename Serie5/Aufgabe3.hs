pairsPot :: Integer -> Integer -> [Integer]
pairsPot a b = cutList (sort(makeList a b))

makeList :: Integer -> Integer -> [Integer]
makeList a b = [x^y | x <- [2..a], y <- [2..b]]

cutList :: [Integer] -> [Integer]
cutList [] = []
cutList [a] = [a]
cutList (x:xs) | x == head xs = x : cutList(tail(xs))
             | otherwise = x : cutList(xs)
