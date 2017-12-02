{-Dadurch, dass das erste und letzte Element nicht in Betracht kommen
können wir die Betrachtung wie hier geschehen einschränken. Sind weniger als
drei Elemente vorhanden gibt es per ungeschriebener Definition keine Extrema.
Falls die Funktion wieder angewendet werden sollte auf eine leere Liste sind
wir ebenfalls fertig.-}
listOfLocalMaxs :: Ord a => [a] -> [a]
listOfLocalMaxs [] = []
listOfLocalMaxs (x:y:xs)
    | xs == [] = []
    | (y > x && y > head xs && xs /= []) = y : listOfLocalMaxs(y:xs)
    | (y > x && y > head xs) = [] ++ [y]
    | xs == [] = []
    | otherwise  = listOfLocalMaxs(y:xs)
