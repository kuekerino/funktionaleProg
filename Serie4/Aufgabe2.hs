{-Wir bedienen uns der Funktionen aus der ersten Aufgabe. Per List
Comprehension erstellen wir uns direkt die Liste, die das erfüllt, was wir
wollen.
-}
allFriendsSmaller :: Int -> [(Int,Int)]
allFriendsSmaller n = [(x,y) | x <- [1..(n-2)],
                               y <- [1..(n-1)],
                               x < y,
                               y < n,
                               friends x y]
{- In dieser Hilfsfunktion verknüpfen wir die Aufgaben aus Teil so, dass sie
die Anforderungen an befreundete Zahlen erfüllen.
-}
friends :: Int -> Int -> Bool
friends x y = if (sum $ removelast $ divisors x) == y &&
                 (sum $ removelast $ divisors y) == x
  then True
  else False

divisors :: Int -> [Int]
divisors n = nichtteileraussortierer n [1 .. n]

nichtteileraussortierer :: Int -> [Int] -> [Int]
nichtteileraussortierer _ [] = []
nichtteileraussortierer y (x:xs)
    | y `mod` x /= 0 = nichtteileraussortierer y xs
    | otherwise = x : nichtteileraussortierer y xs

removelast :: [Int] -> [Int]
removelast (x:xs)
    | xs == [] = []
    | otherwise = [x] ++ removelast xs
