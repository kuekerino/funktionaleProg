pythTripelsSmaller :: Int -> [(Int, Int, Int)]
pythTripelsSmaller n = aussortieren n [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n]]

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
