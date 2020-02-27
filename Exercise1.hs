module Exercise1 where

import Data.List ( (\\) )
import Data.Maybe
import Test.QuickCheck

-- doctest path: export PATH="$HOME/Library/Haskell/bin:$PATH"

type Bag a = [(a,Int)]

-------------------------------
-- Exercise a
-------------------------------

ins :: Eq a => a -> Bag a -> Bag a
ins n [] = [(n,1)]
ins n ((x,y):b) | n == x = (x,y+1) : b
                | otherwise = (x,y) : ins n b

del :: Eq a => a -> Bag a -> Bag a
del _ [] = []
del n ((x,y):b) | n == x && y == 1 = del n b
                | n == x && y /= 1 = (x,y-1) : del n b
                | otherwise = (x,y) : del n b

-------------------------------
-- Exercise b
-------------------------------

bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

-------------------------------
-- Exercise c
-------------------------------

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((x,y):z) a | lookup x a >= Just y = subbag z a
                   | otherwise = False

-------------------------------
-- Exercise d
-------------------------------

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] _ = []
isbag ((x,y):z) a | m == Nothing = isbag z a
                  | m >= Just y = (x,y) : isbag z a
                  | m <= Just y = (x,fromJust (m)) : isbag z a
      where m = lookup x a

-------------------------------
-- Exercise e
-------------------------------

size :: Bag a -> Int
size [] = 0
size ((x,y):b) = y + size b

-------------------------------
-- Test Cases
-------------------------------

-- | Compute sub numbers
--
-- Examples
-- >>> ins 3 [(1,2),(3,5),(6,7)]
-- [(1,2),(3,6),(6,7)]
--
-- >>> ins 3 [(1,2),(7,8)]
-- [(1,2),(7,8),(3,1)]
--
-- >>> ins 3 []
-- [(3,1)]
-- 
--
--
-- >>> del 3 [(4,5),(9,1)]
-- [(4,5),(9,1)]
--
-- >>> del 3 [(4,5),(3,1)]
-- [(4,5)]
--
-- >>> del 3 [(4,5),(3,2)]
-- [(4,5),(3,1)]
--
-- >>> del 3 []
-- []
--
--
--
-- >>> bag [2,3,3,5,7,7,7,8]
-- [(8,1),(7,3),(5,1),(3,2),(2,1)]
--
-- >>> bag [5,5,2,7,7,2]
-- [(2,2),(7,2),(5,2)]
--
--
--
-- >>> subbag [(4,5),(5,5)] [(5,5),(4,5)]
-- True
--
-- >>> subbag [(3,3)] [(3,3),(9,1)]
-- True
--
-- >>> subbag [(3,3),(9,1)] [(3,3)] 
-- False
--
-- >>> subbag [(3,1),(6,7)] [(6,7),(3,2)]
-- True
--
--
--
-- >>> isbag [(1,2),(3,4)] [(1,4),(5,6)]
-- [(1,2)]
--
-- >>> isbag [(1,2),(3,4)] [(5,6),(1,4)]
-- [(1,2)]
--
-- >>> isbag [(2,3),(5,6)] [(9,1),(1,4)]
-- []
--
-- >>> isbag [(3,2),(6,7)] [(3,1),(6,1)]
-- [(3,1),(6,1)]
--
-- >>> isbag [(9,1),(4,5)] [(4,3),(9,1)]
-- [(9,1),(4,3)]
--
--
--
-- >>> size [(2,3),(3,4)]
-- 7










