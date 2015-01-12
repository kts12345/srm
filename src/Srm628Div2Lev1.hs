-- Srm628Div2Lev1 BishopMove
-- http://community.topcoder.com/stat?c=problem_statement&pm=13280
module Srm628Div2Lev1 where
------------------------------------------------------
distinctParity x y  =  odd (x + y)
------------------------------------------------------
bishopMove r1 c1 r2 c2
  |  distinctParity x y  = -1   -- distinct parity
  |  (x,y) == (0,0)      =  0   -- same     parity - origin
  |  abs x == abs y      =  1   -- same     parity - same absolute value
  |  otherwise           =  2   -- same     parity - otherwise
  where
      (x,y) = (r2-r1, c2-c1)    -- normalize
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