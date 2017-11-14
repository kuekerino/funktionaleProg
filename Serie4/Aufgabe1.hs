divisors :: Int -> [Int]
divisors n = filtertmp n [1 .. n]

trueDivisors :: [(Int,[Int])]
trueDivisors = [(1, filtertmp 1 [1] ),(2,filtertmp 2 [2] ) ]

primes :: [Int]
primes = [1,2,3,4]

filtertmp :: Int -> [Int] -> [Int]
filtertmp _ [] = []
filtertmp y (x:xs)
    | y `mod` x /= 0 = filtertmp y xs
    | otherwise = x : filtertmp y xs
