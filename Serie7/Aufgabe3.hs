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


mapTree :: (Ord a, Ord b) => (a -> b) -> BSearchTree a -> BSearchTree b
mapTree f Nil = Nil
mapTree f (Node x ltree rtree) = Node (f x) (mapTree f ltree) (mapTree f rtree)



foldTree :: (Ord a) => b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree b f Nil = b
foldTree b f (Node x ltree rtree) = f x (foldTree b f ltree) (foldTree b f rtree)


successor :: (Ord a) => a -> BSearchTree a -> Maybe a
successor b Nil = Nothing
successor b (Node x ltree rtree)
    | b < x && ltree /= Nil = successor b ltree
    | b < x && ltree == Nil = Just x
    | b > x && rtree /= Nil = successor b rtree
    | b > x && rtree == Nil = Just x
    | b == x && rtree == Nil = Just x
    | b == x && rtree /= Nil = successor b rtree

smallest:: (Ord a) => BSearchTree a -> a
smallest (Node x Nil _) = x
smallest (Node x leftTree _) = smallest leftTree


biggest:: (Ord a) => BSearchTree a -> a
biggest(Node x _ Nil) = x
biggest(Node x _ rTree) = biggest rTree


mirror:: (Ord a) => BSearchTree a -> BSearchTree a
mirror Nil = Nil
mirror (Node x xl xr) = Node x (mirror xr) (mirror xl)

inOrder :: (Ord a) => BSearchTree a -> [a]
inOrder Nil = []
inOrder (Node x ltree rtree) = inOrder ltree ++ x : inOrder rtree

preOrder :: (Ord a) => BSearchTree a -> [a]
preOrder Nil = []
preOrder (Node x ltree rtree) = x : preOrder ltree ++ preOrder rtree


search :: (Ord a) => a -> BSearchTree a -> Bool
search _ Nil = False
search k (Node x ltree rtree) | k==x = True
    | k<x = search k ltree
    | otherwise = search k rtree

insert :: (Ord a) => a -> BSearchTree a -> BSearchTree a
insert k Nil = Node k Nil Nil
insert k (Node x ltree rtree)
    | k<x = Node x (insert k ltree) rtree
    | otherwise = Node x ltree (insert k rtree)

delete :: (Ord a) => a-> BSearchTree a-> BSearchTree a
delete x Nil = Nil
delete x (Node y ltree rtree)
    | x < y = Node y (delete x ltree) rtree
    | x == y = join ltree rtree
    | x > y = Node y ltree (delete x rtree)

join ::(Ord a) => BSearchTree a->BSearchTree a->BSearchTree a
join xtree Nil = xtree
join xtree ytree = Node e xtree ntree
    where
    (e, ntree) = splitMin ytree
    splitMin (Node x Nil tree) = (x, tree)
    splitMin (Node x ltree rtree) = (f, Node x mtree rtree)
        where
        (f, mtree) = splitMin ltree

nodes :: (Ord a) => BSearchTree a -> Integer
nodes Nil = 0
nodes (Node x leftT rightT) = 1 + nodes leftT + nodes rightT

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
