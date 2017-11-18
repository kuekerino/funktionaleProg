{- Wir wenden einfach die übergeben Funktion rekursiv auf dem Rest des
eingegebenen Arrays an. Wenn der zweite Teil leer ist sind wir am Ende
der Liste und geben wahr zurück. Eine Begrenzung des Programmes ist, dass
eingegebene Listen der Länge 1 immer wahr zurück geben, egal welchen
Vergleichsoperator man eingibt.
-}
isSorted :: (Int -> Int -> Bool) -> [Int] -> Bool
isSorted x (y:ys)
    | ys == [] = True
    | x y (head ys) = isSorted x ys
    | otherwise = False
