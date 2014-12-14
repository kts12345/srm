-- Srm631Div2Lev2 catsOnTheLine
-- http://community.topcoder.com/stat?c=problem_statement&pm=13392
module Srm631Div2Lev2 where
import Data.Maybe
import Data.List
import Control.Monad
------------------------------------------------------
cJust cond value = if cond then (Just value) else Nothing
------------------------------------------------------
handlerS prev_end (lo,hi,cnt) = cJust isPossible end
    where (start,end) = (max (prev_end+1) lo, start+cnt-1)
          isPossible  = end <= hi
------------------------------------- -----------------
handler xs x  = cJust isPossible xs'
    where xs'        = insert x xs  -- if xs is sorted then xs' is sorted!! see insert
          isPossible = isJust $ foldM handlerS (-2001) xs'
------------------------------------------------------
catsOnTheLine ps cs time = toString $ foldM handler [] events
    where events     = [(p-time, p+time, cnt)|(p,cnt)<-zip ps cs]
          toString v = if isJust v then "Possible" else "Impossible"
------------------------------------------------------
main = do
        print $ catsOnTheLine [0] [7] 3
        print $ catsOnTheLine [0] [8] 2
        print $ catsOnTheLine [0,1] [3,1] 0
        print $ catsOnTheLine [5, 0, 2] [2, 3, 5] 2
        print $ catsOnTheLine [5, 1, -10, 7, 12, 2, 10, 20] [3, 4, 2, 7, 1, 4, 3, 4] 6

