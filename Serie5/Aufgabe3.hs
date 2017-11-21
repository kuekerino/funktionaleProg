potList :: Int -> Int -> [Int]
potList x y = unique $ [a^b | a <- [2..x],
                     b <- [2..y]]

unique :: [Int] -> [Int]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]
