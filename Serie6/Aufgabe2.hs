largestSeq :: Ord a => [a] -> [a]
largestSeq a = comparer $ quicksort (splitter a)

splitter :: Ord a => [a] -> [[a]]
splitter (x:xs)
    | xs /= []  = (x:xs) : splitter xs
    | otherwise = [[x]]

comparer :: Ord a => [[a]] -> [a]
comparer [] = []
comparer (x:xs) = hilfsfunktion x (head xs)


hilfsfunktion :: Ord a => [a] -> [a] -> [a]
hilfsfunktion a b = 

{-Kleine Veraenderung der Funktion aus der Vorlesung, da die Funktion
aus der Vorlesung fehlerhaft ist und so wie sie gegeben ist nicht das
gewuenschte Ergebnis gibt.-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort smaller) ++ x : (quicksort bigger)
    where
        smaller = [ y | y <- xs, y<x ]
        bigger = [ y | y <- xs, y>=x ]
