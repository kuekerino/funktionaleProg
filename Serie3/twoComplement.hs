twoComplement :: [Int] -> [Int]
twoComplement x = map addstuff (map flipValue x)

flipValue :: Int -> Int
flipValue x = if x == 0
    then 1
    else 0

addstuff :: Int -> Int
addstuff x =  (x + 1) `mod` 2
