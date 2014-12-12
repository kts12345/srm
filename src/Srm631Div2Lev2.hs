module Srm631Div2Lev2 where
import Data.Maybe
import Data.List
import Control.Monad
------------------------------------------------------
cJust value cond = if cond then Just value else Nothing
------------------------------------------------------
handlerS  prev_end (lo,hi,cnt) = cJust end (end<=hi)
    where (start,end) = (max (prev_end+1) lo, start+cnt-1)
------------------------------------- -----------------
handler xs (time,pos,cnt) = cJust xs' $ isJust (foldM handlerS (-2001) xs')
    where xs' = insert (pos-time, pos+time, cnt) xs  -- if xs is sorted then xs' is sorted!! see insert
------------------------------------------------------
catsOnTheLine ps cs time = if move then "Possible" else "ImPossible"
    where move = isJust $ foldM handler [] $ zip3 [time,time..] ps cs
------------------------------------------------------
main = do
        print $ catsOnTheLine [0] [7] 3
        print $ catsOnTheLine [0] [8] 2
        print $ catsOnTheLine [0,1] [3,1] 0
        print $ catsOnTheLine [5, 0, 2] [2, 3, 5] 2
        print $ catsOnTheLine [5, 1, -10, 7, 12, 2, 10, 20] [3, 4, 2, 7, 1, 4, 3, 4] 6

