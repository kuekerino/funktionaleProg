{-Wir haben noch die Eq Klasse dazugenommen.-}
data SimpleBT = L | N SimpleBT SimpleBT deriving Show
type Height = Integer

instance Eq SimpleBT where
  (==) (L) (L) = True
  (==) _ _ = False

{-funktioniert nur fuer Baeume mit mindestens 2 Blaettern.-}
insertLeaves :: Integer -> SimpleBT -> SimpleBT
insertLeaves x y
    | x < 0 = error "kann keine negative Anzahl Blaetter hinzufuegen"
    | x == 0 = y
    | otherwise = insertLeaves (x-1) (singleLeafInsert y)
 -- eine eingegebene Anzahl von Blättern wird eingefügt.

deleteLeaves :: Integer -> SimpleBT -> SimpleBT
deleteLeaves x y
    | x < 0 = error "kann keine negative Anzahl Blaetter entfernen"
    | x == 0 = y
    | otherwise = deleteLeaves (x-1) (singleLeafDelete y)
 -- eine eingegebene Anzahl von Blättern wird gelöscht.

height :: SimpleBT -> Integer
height L = 0
height (N lt rt) = (max (height lt) (height rt)) + 1

{-Es wird der am wenigsten Hohe Teil gesucht und dort ein Blatt eingefuegt.
Dies wird so oft getan wie in der Aufgabe gefordert.-}
singleLeafInsert :: SimpleBT -> SimpleBT
singleLeafInsert (N ltree rtree)
    | ltree == L = N (N L L) rtree
    | rtree == L = N ltree (N L L)
    | (height ltree) < (height rtree) = N (singleLeafInsert ltree) rtree
    | otherwise = N ltree (singleLeafInsert rtree)

{-Es wird der tiefste Zweig gesucht und hier das rechteste Blatt geloescht.-}
singleLeafDelete :: SimpleBT -> SimpleBT
singleLeafDelete (N ltree rtree)
    | height ltree == 0 && height rtree == 0 = error "hier koennen wir nichts loeschen"
    | height rtree == 1 && height ltree == 1 = N ltree L
    | height ltree > 1 && height rtree == 0 = N (singleLeafDelete ltree) rtree
    | height ltree == 0 && height rtree > 1 = N ltree (singleLeafDelete rtree)
    | height rtree == 1 = N ltree L
    | height ltree == 1 = N L rtree
    | height ltree > height rtree = N (singleLeafDelete ltree) rtree
    | otherwise = N ltree (singleLeafDelete rtree)
