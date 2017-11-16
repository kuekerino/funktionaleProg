divisors :: Int -> [Int]
divisors n = filtertmp n [1 .. n]

trueDivisors :: [(Int,[Int])]
trueDivisors = zip x y
    where x = [1..]
          y = map removelast z
          z = map divisors [1..]

primes :: [Int]
primes = [1,2,3]

filtertmp :: Int -> [Int] -> [Int]
filtertmp _ [] = []
filtertmp y (x:xs)
    | y `mod` x /= 0 = filtertmp y xs
    | otherwise = x : filtertmp y xs

removelast :: [Int] -> [Int]
removelast (x:xs)
    | xs == [] = []
    | otherwise = [x] ++ removelast xs
