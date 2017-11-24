{-Rekursiv verschachtelte Funktion, die sich immer wieder selbst aufruft,
mit jeweils anderen Parametern, je nachdem ob das bestimmte Pattern gefunden
wurde oder nicht.
-}
diffList :: Ord a => [a] -> [a] -> [a]
diffList x [] = x
diffList [] _ = []
diffList (x:xs) (y:ys)
    | x == y = diffList xs ys
    | otherwise = diffList [x] ys ++ diffList xs (y:ys)
    
    
{-
diffList :: Ord a => [a] -> [a] -> [a]
diffList x [] = x
diffList [] _ = []
diffList (x:xs) (y:ys) 
 | not (x `elem` (y:ys))  = x : diffList xs (y:ys)
 | otherwise   = diffList xs (delete x (y:ys))
-}
