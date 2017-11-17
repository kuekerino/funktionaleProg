divisors :: Int -> [Int]
divisors n = filtertmp n [1 .. n]

trueDivisors :: [(Int,[Int])]
trueDivisors = zip x y
    where x = [1..]
          y = map removelast z
          z = map divisors [1..]

primes :: [Int]
primes = nullenRausschmeisser 0 (map auswahl trueDivisors)

filtertmp :: Int -> [Int] -> [Int]
filtertmp _ [] = []
filtertmp y (x:xs)
    | y `mod` x /= 0 = filtertmp y xs
    | otherwise = x : filtertmp y xs

nullenRausschmeisser :: Int -> [Int] -> [Int]
nullenRausschmeisser _ [] = []
nullenRausschmeisser y (x:xs)
    |  x == 0 = nullenRausschmeisser y xs
    | otherwise = x : nullenRausschmeisser y xs

removelast :: [Int] -> [Int]
removelast (x:xs)
    | xs == [] = []
    | otherwise = [x] ++ removelast xs


auswahl :: (Int,[Int]) -> Int
auswahl (x,y) = if length y == 1
    then x
    else 0
