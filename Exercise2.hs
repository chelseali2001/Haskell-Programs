module Exercise2 where

import Data.Char

type Bag a = [(a,Int)]  

ph :: (Eq a, Ord a, Num a, Show a) => Bag a -> String
ph x = row 0 (find x) x

-- change rows / creates x and y axis
row :: (Eq a, Ord a, Num a, Show a) => a -> Int -> Bag a -> String
row c r x | c == 0 && r /= 0 = show r ++ row (c + 1) r x
          | r > 0 && c <= 9 = pixel c r x : row (c + 1) r x
          | r > 0 && c > 9 = "\n" ++ row 0 (r - 1) x
          | r == 0 && c == 0 = ' ' : row (c + 1) r x 
          | otherwise = "123456789" ++ "\n"

-- change columns
pixel :: (Eq a, Ord a, Num a, Show a) => a -> Int -> Bag a -> Char
pixel c r x | look c x < r = ' '
            | otherwise = 'X'

-- checking coordinates
look :: (Eq a, Ord a, Num a, Show a) => a -> Bag a -> Int
look c [] = 0
look c ((x,y):b) | x == c = y
                 | otherwise = look c b

-- finds highest int val in bag
find :: (Eq a, Ord a, Num a, Show a) => Bag a -> Int
find [(x,y)] = y
find ((x,y):z) = max y (find z)

-- | Compute sub numbers
--
-- Examples
-- >>> putStr (ph [(5,1),(7,3),(2,1),(3,2),(8,1)])
-- 3      X
-- 2  X   X
-- 1 XX X XX
--  12345678
