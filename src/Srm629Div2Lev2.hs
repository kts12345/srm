-- Srm629Div2Lev2 CandyMaking
-- http://community.topcoder.com/stat?c=problem_statement&pm=13340
------------------------------------------------------
module Srm629Div2Lev2 where
import Data.List
------------------------------------------------------
handlerUpdateTable (sortedList, volumeTotal) (density, volume) =
                   (insert (density, volume) sortedList, volumeTotal + volume)
------------------------------------------------------
handlerFindOptimal (sortedList, volumeTotal) = (sortedList, optimal)
    where optimal = fst.head                                $ -- 4. extract first density
                    dropWhile (\(_,s) -> s < volumeTotal/2) $ -- 3. [(density, _) |  median < accumumlate[volume]]
                    scanl1    (\(_,s) (d,v) -> (d, s+v))    $ -- 2. [(density, accumulate [volume])]
                    sortedList                                -- 1. [(density, volume)]
------------------------------------------------------
handlerFindDiffsum (list, optimal) = sum [abs(d-optimal)*v | (d,v)<-list]
------------------------------------------------------
candyMaking containerVolumes desiredWeights = last diffsum
    where                                        -- # | time      | space | describe
      diffsum =                                  -----------------------------------------------------------------
        map        handlerFindDiffsum         $    -- 5.|O(n),E(n/2)| O(n)  | [sum (optimal-density)*volume]
        map        handlerFindOptimal         $    -- 4.|O(n),E(n/2)| O(n)  | [(sort [(density, volume)], optimal)]     where optimal     = median
        tail.scanl handlerUpdateTable ([],0)  $    -- 3.|O(log n)   | O(n)  | [(sort [(density, volume)], totalVolume)] where totalVolume = sum [volume]
        map        (\(v,w)->(w/v, v))         $    -- 2.|O(1)       | O(1)  | [(density, volume)]                       where density     = weight/volume
        zip        containerVolumes desiredWeights -- 1.|O(1)       | O(1)  | [(volume, weight)]
------------------------------------------------------
main = do
    print $ candyMaking [5] [1000]
    print $ candyMaking [10,10] [1000,2000]
    print $ candyMaking [10,20,40] [4000,2000,1000]
    print $ candyMaking [1234,1541,3321,1234,123,123,3414,123,12,2442,1421,1223,3232,1123,2121] [3213,1231,232143,44312,132132,142424,123123,41341,41244,21312,232131,2312,2322,11,2223]
    print $ candyMaking [30621,30620,2] [1,1,1000000]
{- Output
0.0
1000.0
5250.0
983673.2727272727
999999.9999673415
-}

------------------------------------------------------
-- if you need more simple code for batch-job
candyMaking' containerVolumes desiredWeights = diffsum
    where  inputs      = sort     $ zipWith  (\v w->(w/v, v)) containerVolumes desiredWeights
           volumeTotal = sum      $ map snd inputs
           optimal     = fst.head $ dropWhile (\(_,s) -> s < volumeTotal/2) $ scanl1 (\(_,s) (d,v) -> (d, s+v)) inputs
           diffsum     = sum      $ map (\(d,v)->abs(d-optimal)*v) inputs