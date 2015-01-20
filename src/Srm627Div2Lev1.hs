-- Srm627Div2Lev1 ManySquare
-- http://community.topcoder.com/stat?c=problem_statement&pm=13277&rd=16008
module Srm627Div2Lev1 where
------------------------------------------------------
import qualified Data.Map as M
------------------------------------------------------
handler table stick | cnt == 3  = (1, M.delete stick         table)
                    | otherwise = (0, M.insert stick (cnt+1) table)
    where cnt = M.findWithDefault 0 stick table
------------------------------------------------------
manySquares xs = 
    last                        $ -- 4.                                                                 1
    scanl (+) 0                 $ -- 3.[ 0       ,  0       ,  0           ,  0,             1       ,  1        ]
    map   fst                   $ -- 2.[ 0       ,  0       ,  0           ,  0,          ,  1       ,  0        ]
    scanl handler' (0, M.empty) $ -- 1.[(0,"1:1"), (0,"1:2"), (0,"1:2,5:1"), (0,"1:3,5:1"), (1,"5:1"), (0, "5:2")]
    xs                            -- 0.[  1     ,    1    ,     5          ,   1          ,   1       ,  5       ]
    where
        handler' (_, t) = handler t -- sytax adaptor
------------------------------------------------------
main = do
    print $ manySquares  [1,1,2,2,1,1,2]
    print $ manySquares  [3,1,4,4,4,10,10,10,10]
    print $ manySquares  [1,2,3,4,1,2,3,4,1,2,3,1,2,3,4,1,2,3,3,3]
    print $ manySquares  [1,1,1,2,2,2,3,3,3,4,4,4]
    print $ manySquares  [1,1,1,2,1,1,1,3,1,1,1]
    print $ manySquares  [2,2,4,4,8,8]
{- Output
 1
 1
 3
 0
 2
 0
-}
------------------------------------------------------
-- last.scanl (+) 0  == sum
------------------------------------------------------
-- if you need more simple code for batch-job.
manySquares' xs =
    M.fold  (+) 0       $
    M.map   truncate    $
    M.map   (/4)        $
    M.fromListWith (+)  $
    zip xs [1,1..]
-- or
manySquares'' xs =
    M.fold (\v acc-> (div v 4) + acc) 0 $
    M.fromListWith (+)                  $
    zip xs [1,1..]