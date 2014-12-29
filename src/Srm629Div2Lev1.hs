-- Srm629Div2Lev1 rectangleCoveringEasy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13363
------------------------------------------------------
module Srm629Div2Lev1 where
------------------------------------------------------
rectangleCoveringEasy holeH holeW boardH boardW
    | minH >  minB || maxH >  maxB = -1
    | minH == minB && maxH == maxB = -1
    | otherwise                    =  1
    where minmax  x y = (min x y, max x y)
          (minH, maxH) = minmax holeH  holeW
          (minB, maxB) = minmax boardH boardW
------------------------------------------------------
main = do
    print $ rectangleCoveringEasy  1  1  1  1
    print $ rectangleCoveringEasy  3  5  4  6
    print $ rectangleCoveringEasy 10 20 25 15
    print $ rectangleCoveringEasy  3 10  3 12
------------------------------------------------------
{- Output
-1
 1
 1
 1
-}
