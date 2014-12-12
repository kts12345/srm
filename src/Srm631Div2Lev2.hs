module Srm631Div2Lev2 where
import Data.Maybe
import Data.List
import Control.Monad
------------------------------------------------------
cJust value cond = if cond then Just value else Nothing
------------------------------------------------------
handlerS  start (low, high, cnt) = cJust (end'+1) (end' <= high)
    where (start',end') = (max start low, start' + cnt-1)
------------------------------------- -----------------
handler (time,xs) (pos,cnt) = cJust  (time,xs') $
                              isJust (foldM handlerS (-2001) xs')
  where xs' = sort $ xs++[(pos-time,pos+time,cnt)]
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

