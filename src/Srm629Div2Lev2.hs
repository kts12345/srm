-- Srm629Div2Lev2 CandyMaking
-- http://community.topcoder.com/stat?c=problem_statement&pm=13340
------------------------------------------------------
module Srm629Div2Lev2 where
import Data.List
import Data.Maybe
------------------------------------------------------
handlerInsertTable (densities, median) (density, volume) =
                   (insert (density, volume) densities, median + volume/2)
------------------------------------------------------
handlerFindOptimal (densities, median) = (densities, optimal)
    where subsums = scanl1 (\(_,s) (d,v) -> (d, s+v)) densities
          optimal = head [d|(d,s)<-subsums, median <= s]
------------------------------------------------------
handlerFindDiffsum (densities, optimal) = sum [abs(d-optimal)*v | (d,v)<-densities]
------------------------------------------------------
candyMaking volumes weights = last diffsum
    where                                 -- # | time      | space | describe
      diffsum =                           -----------------------------------------------------------------
        map   handlerFindDiffsum        $ -- 5.|O(n),E(n/2)| O(n)  | [sum (optimal-density)*volume]
        map   handlerFindOptimal        $ -- 4.|O(n),E(n/2)| O(n)  | [(sort [(density, volume)], optimal)] where optimal = median
        scanl handlerInsertTable ([],0) $ -- 3.|O(n),E(n/2)| O(n)  | [(sort [(density, volume)], median)]  where median  = sum[volume]/2
        map   (\(v,w)->(w/v, v))        $ -- 2.|O(1)       | O(1)  | [(density, volume)]                   where density = weight/volume
        zip   volumes weights             -- 1.|O(1)       | O(1)  | [(volume, weight)]
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
-- if you need more simple code for batch-job. O(n * log n)

candyMaking' volumes weights = sum [abs (d-optimal) * v| (d,v) <-densities]
    where  densities   = [(w/v, v)|(v,w) <-zip volumes weights]
           densities'  = scanl1 (\(_,s) (d,v) -> (d, s+v)) $ sort densities
           optimal     = head [d|(d,s)<-densities', (sum volumes)/2 <= s]

------------------------------------------------------
-- refactoring pudae's code
candyMaking'' vs ws  = minimum [sum [abs(w-v*d) | (w,v) <- zip ws vs] | d <- zipWith (/) ws vs]