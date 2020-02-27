module Exercise3 where

import Data.List ( (\\) )

type Number = Int

type Point = (Number, Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point, Point)

-------------------------------
-- Exercise a
-------------------------------

width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = 2 * r
width (Rect _ x _) = x

bbox :: Shape -> BBox
bbox (Pt p) = (p,p)
bbox (Circle (x,y) r) = ((x - r, y - r) , (x + r, y + r))
bbox (Rect (x,y) l w) = ((x,y) , (x + l, y + w))

minX :: Shape -> Number
minX x = fst (fst (bbox (x)))

-------------------------------
-- Exercise b
-------------------------------

move :: Shape -> Point -> Shape
move (Pt (x,y)) (a,b) = Pt (x + a, y + b)
move (Circle (x,y) r) (a,b) = Circle (x + a, y + b) r
move (Rect (x,y) l w) (a,b) = Rect (x + a, y + b) l w

-------------------------------
-- Exercise c
-------------------------------

alignLeft :: Figure -> Figure
alignLeft [] = []
alignLeft xs = map (moveToX x) xs
          where x = findmin xs

findmin :: Figure -> Number
findmin [x] = minX (x)
findmin (x:xs) = min (minX (x)) (findmin xs)

moveToX :: Number -> Shape -> Shape
moveToX n (Circle (x,y) r) = Circle (n,y) r
moveToX n x = move x (-(minX x-n),0) 

-------------------------------
-- Exercise d
-------------------------------

inside :: Shape -> Shape -> Bool
inside x y | bbox (x) == bbox (y) = True
           | width (y) < width (x) = check (bbox (y)) (bbox (x))
           | otherwise = check (bbox (x)) (bbox (y))

check :: BBox -> BBox -> Bool
check ((x,y),(x0,y0)) ((a,b),(a0,b0)) | a <= x && x <= a0 && b <= y && y <= b0 && a <= x0 && x0 <= a0 && b <= y0 && y0 <= b0 = True
                                      | otherwise = False

-------------------------------
-- Test Cases
-------------------------------     

-- | Compute sub numbers
--
-- Examples
-- >>> map width [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- [0,6,7]
--
--
--
-- >>> map bbox [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- [((4,4),(4,4)),((2,2),(8,8)),((3,3),(10,5))]
--
--
--
-- >>> map minX [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
-- [4,2,3]
--
--
--
-- >>> move (Pt (4,4)) (1,1)
-- Pt (5,5)
--
-- >>> move (Circle (5,5) 3) (1,1)
-- Circle (6,6) 3
--
-- >>> move (Rect (3,3) 7 2) (1,1)
-- Rect (4,4) 7 2
--
--
--
-- >>> alignLeft [(Pt (4,4)), (Circle (5,5) 3), (Rect (3,3) 7 2)]
-- [Pt (2,4),Circle (2,5) 3,Rect (2,3) 7 2] 
--
--
--
-- >>> inside (Pt (4,4)) (Pt (4,4))
-- True
--
-- >>> inside (Pt (4,4)) (Pt (5,5))
-- False
-- 
-- >>> inside (Pt (4,4)) (Circle (5,5) 3)
-- True
--
-- >>> inside (Pt (4,4)) (Circle (2,4) 1)
-- False
--
-- >>> inside (Pt (4,4)) (Rect (3,3) 2 2)
-- True
-- 
-- >>> inside (Circle (3,4) 1) (Circle (0,4) 2)
-- False
--
-- >>> inside (Circle (3,4) 2) (Circle (3,4) 1)
-- True
--
-- >>> inside (Circle (3,4) 2) (Rect (2,3) 4 2)
-- False
--
-- >>> inside (Circle (3,4) 2) (Rect (2,3) 2 2)
-- True
--
-- >>> inside (Rect (0,0) 4 4) (Rect (2,3) 4 2)
-- False
--
-- >>> inside (Rect (3,3) 1 1) (Rect (2,3) 4 2)
-- True








