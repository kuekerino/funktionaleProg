{-Zuerst wird eine Liste erstellt, alle in Frage kommenden Zahlen beinhaltet,
dann werden alle Zahlen aussortiert, bei denen die Modulo Funktion einen
Rest ungleich Null ergibt.
-}
divisors :: Int -> [Int]
divisors n = nichtteileraussortierer n [1 .. n]

{-Mit Hilfe der divisors Funktion wird hier für jede Zahl eine Liste aller
Teiler erstellt, danach wird mit der removelast Funktion das letzte Element
entfernt, da wir nur echte Teiler wollen und das letzte Element immer die
Zahl selber ist.
-}
trueDivisors :: [(Int,[Int])]
trueDivisors = zip x y
    where x = [1..]
          y = map removelast z
          z = map divisors [1..]

{-Wir erstellen uns per trueDivisors eine Liste, die uns das gewünschte liefert,
danach haben wir eine Liste, deren Einträge entweder Primzahlen oder die Zahl
Null ist. Mit auswahl sortieren wir dann die Nullen aus.
-}
primes :: [Int]
primes = nullenRausschmeisser 0 (map auswahl trueDivisors)

nichtteileraussortierer :: Int -> [Int] -> [Int]
nichtteileraussortierer _ [] = []
nichtteileraussortierer y (x:xs)
    | y `mod` x /= 0 = nichtteileraussortierer y xs
    | otherwise = x : nichtteileraussortierer y xs

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

{-
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], rem n x == 0]

trueDivisors :: [(Int,[Int])]
trueDivisors = [(x, init y) | x <- [1..], y <- [divisors x]]

primes :: [Int]
primes = [x | x <- [1..], (x, [1]) `elem` take x trueDivisors]
-}
