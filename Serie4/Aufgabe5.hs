{-Wir erstellen uns eine Liste aller Zahlentripel mit Zahlen, die in Frage
kommen. Danach sortieren wir die Tripel aus, die einer der
Bedingungen widersprechen.
-}
pythTripelsSmaller :: Int -> [(Int, Int, Int)]
pythTripelsSmaller n = aussortieren n [(x,y,z) | x <- [1..n],
                                                 y <- [1..n],
                                                 z <- [1..n]]

aussortieren :: Int -> [(Int,Int,Int)] -> [(Int,Int,Int)]
aussortieren _ [] = []
aussortieren n (x:xs)
    |  ((first x)^2 + (second x)^2 == (third x)^2 &&
       first x  <= second x &&
       second x <= third x  &&
       third x  <= n) = x : aussortieren n xs
    | otherwise =  aussortieren n xs


first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


{-
pythTriplesSmaller :: Int -> [(Int,Int,Int)]
pythTriplesSmaller n = [(a,b,c) | a <- [1..n-1], b <- [1..n-1], c <- [1..n-1], a<b, 
 b<c, (c^2) == (a^2)+(b^2)]
-}
