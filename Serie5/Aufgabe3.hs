potList :: Int -> Int -> [Int]
potList x y = unique $ [1] ++ [a^b | a <- [2..x],
                     b <- [1..y]]

unique :: [Int] -> [Int]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]
