data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a)
    deriving ( Show, Eq )


postOrder :: (Ord a) => BSearchTree a -> [a]
postOrder Nil = []
postOrder (Node x ltree rtree) = postOrder ltree ++ postOrder rtree ++ [x]

-- entscheidet, ob jeder Knoten des Baums genau zwei Kinder hat oder nicht
twoChildren :: (Ord a) => BSearchTree a -> Bool
twoChildren Nil = error "undefined"
twoChildren (Node x ltree rtree)
    | ltree == Nil && rtree == Nil = True
    | ltree /= Nil && rtree == Nil = False
    | ltree == Nil && rtree /= Nil = False
    | otherwise = twoChildren ltree && twoChildren rtree

{-Nach Definition aus den Folien.-}
full :: (Ord a) => BSearchTree a -> Bool
full b = innerNodes b == (2^(height b)) - 1 &&
                              leafs b == 2^height b

{-Aufbauend auf der Standarddefinition von map, angepasst fuer die Struktur.-}
mapTree :: (Ord a, Ord b) => (a -> b) -> BSearchTree a -> BSearchTree b
mapTree f Nil = Nil
mapTree f (Node x ltree rtree) = Node (f x) (mapTree f ltree) (mapTree f rtree)

{-Aufbauend auf der Standarddefinition von fold, angepasst fuer die Struktur.-}
foldTree :: (Ord a) => b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree b f Nil = b
foldTree b f (Node x ltree rtree) = f x (foldTree b f ltree) (foldTree b f rtree)

{-Sucht den Nachbarn, wenn der aktuelle Wert groesser links eher links,
ansonsten eher rechts.-}
successor :: (Ord a) => a -> BSearchTree a -> Maybe a
successor b Nil = Nothing
successor b (Node x ltree rtree)
    | b < x && ltree /= Nil = successor b ltree
    | b < x && ltree == Nil = Just x
    | b > x && rtree /= Nil = successor b rtree
    | b > x && rtree == Nil = Just x
    | b == x && rtree == Nil = Just x
    | b == x && rtree /= Nil = successor b rtree

height :: (Ord a) => BSearchTree a -> Integer
height Nil = 0
height (Node x lt rt) = (max (height lt) (height rt)) + 1

leafs :: (Ord a) => BSearchTree a -> Integer
leafs Nil = 0
leafs (Node x ltree rtree)
    | ltree == Nil && rtree == Nil = 1
    | otherwise = leafs ltree + leafs rtree

innerNodes :: (Ord a) => BSearchTree a -> Integer
innerNodes Nil = 0
innerNodes (Node x ltree rtree)
    | ltree == Nil && rtree == Nil = 0
    | ltree == Nil && rtree /= Nil = 1 + innerNodes rtree
    | ltree /= Nil && rtree == Nil = 1 + innerNodes ltree
    | otherwise = 1 + innerNodes ltree + innerNodes rtree
