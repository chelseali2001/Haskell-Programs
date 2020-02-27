module BinaryTreeBasics where

data Tree a = Empty 
            | Node a (Tree a) (Tree a) 
            deriving (Show)  

data Direction = L 
               | R 
               deriving (Show)

type Directions = [Direction] 

data Crumb a = LeftCrumb a (Tree a) 
             | RightCrumb a (Tree a) 
             deriving (Show)  

type Breadcrumbs a = [Crumb a] 

type Zipper a = (Tree a, Breadcrumbs a)  

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  
-- The tree goes from left to right and downwards
-- Node is the value
-- Empty means that there are no more sublists
-- Use recursion to go to your desired output
  
changeToP :: Directions-> Tree Char -> Tree Char  
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r  
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)  
changeToP [] (Node _ l r) = Node 'P' l r  
-- This changes a value in the third row of the tree
-- Other example:
   -- changeToP :: Tree Char -> Tree Char  
   -- changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)  
-- changeToP l r or changeToP r l is changeToP []


elemAt :: Directions -> Tree a -> a  
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = x  
-- Finds an element in the third row of the tree

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)  
-- Gives you all of the values on the left side of the tree

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)  
-- Gives you all of the values on the left side of the tree

-- goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
-- goLeft (Node _ l _, bs) = (l, L:bs)
-- goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
-- goRight (Node _ _ r, bs) = (r, R:bs)    
-- goLeft (goRight (freeTree, []))
-- (freeTree, []) -: goRight -: goLeft means x -: f = f x or instructions
   -- You have to enter x -: f = f x first

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)    
-- Gives you all of the values and subvalues at a specific starting point

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)  
modify f (Empty, bs) = (Empty, bs) 
-- Changes a specific value in a tree
-- let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree,[])))  
-- let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')
-- let newFocus2 = modify (\_ -> 'X') (goUp newFocus)  
-- let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

attach :: Tree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs)  
-- Creates a new value
-- let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft  
-- let newFocus = farLeft -: attach (Node 'Z' Empty Empty) 

topMost :: Zipper a -> Zipper a  
topMost (t,[]) = (t,[])  
topMost z = topMost (goUp z)  
-- Shows the whole tree including all of the modifications to it

