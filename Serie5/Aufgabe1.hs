{-newList creates a new list with Elements (y * (x ^ Index))
for Example newList 4 [1,2,3,0] => [64,32,16,0] -}
newList :: Int -> [Int] -> [Int]
newList _ [] = []
newList x (y:ys) = y * (x^((length (y:ys))-1)) : newList x ys

{-toDecFrom adds each item of the new list and give the sum-}
toDecFrom :: Int -> [Int] -> Int
toDecFrom x y  = foldl (+) 0 (newList x y)

{-the performance time increases exponantial with the length of the list-}

