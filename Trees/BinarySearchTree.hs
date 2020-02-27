module BinarySearchTree where

data Tree a = Empty
            | Node a (Tree a) (Tree a) 
            deriving (Show, Read, Eq)

treeNode :: a -> Tree a
treeNode item = Node item (Empty) (Empty)

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList items = foldr treeInsert Empty $ reverse items

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node item (left) (right)) = (treeToList left) ++ [item] ++ (treeToList right)

treeRoot :: Tree a -> a
treeRoot (Node item (_) (_)) = item

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert newItem Empty = treeNode newItem
treeInsert newItem (Node item (left) (right)) =
           if newItem < item
              then Node item (treeInsert newItem left) (right)
              else Node item (left) (treeInsert newItem right)
