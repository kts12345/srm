-- Srm628Div2Lev1 BishopMove
-- http://community.topcoder.com/stat?c=problem_statement&pm=13280
module Srm628Div2Lev1 where
------------------------------------------------------
bishopMove r1 c1 r2 c2
  | odd (x+y)      = -1
  | (x,y) == (0,0) =  0
  | abs x == abs y =  1
  | otherwise      =  2
      where (x,y)  = (r2-r1, c2-c1)
------------------------------------------------------
main = do
 print $ bishopMove  4 6 7 3
 print $ bishopMove  2 5 2 5
 print $ bishopMove  1 3 5 5
 print $ bishopMove  4 6 7 4
------------------------------------------------------
{- Output

 1
 0
 2
-1

-}