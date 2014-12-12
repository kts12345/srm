module Srm631Div2Lev2 where
import Data.Maybe
import Data.List
import Control.Monad
------------------------------------------------------
cJust v cond = if cond then Just v else Nothing
------------------------------------------------------
handlerS (time, start) (pos, cnt) = cJust (time,end'+1) possible
  where  (low,   high) = (pos-time, pos+time)
         (start',end') = (max start low, start' + cnt-1)  
         possible      = end' <= high
------------------------------------- -----------------
handler (time,xs) (p,c) = cJust (time,xs ++ [(p,c)])   $
                          isJust                       $
                          foldM handlerS (time, -2001) $ 
                          sort                         $ 
                          xs ++ [(p,c)] 
------------------------------------------------------
catsOnTheLine ps cs time = if possible then "Possible" else "ImPossible"
  where possible = isJust $ foldM handler (time, []) $ zip ps cs
------------------------------------------------------
main = do
        print $ catsOnTheLine [0] [7] 3
        print $ catsOnTheLine [0] [8] 2
        print $ catsOnTheLine [0,1] [3,1] 0
        print $ catsOnTheLine [5, 0, 2] [2, 3, 5] 2
        print $ catsOnTheLine [5, 1, -10, 7, 12, 2, 10, 20] [3, 4, 2, 7, 1, 4, 3, 4] 6

