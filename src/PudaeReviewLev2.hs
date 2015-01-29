module PudaeReviewLev2 where
-------------------------------------------------------------------
import Data.Function
-------------------------------------------------------------------
-- Util : fDiv - Integral Division
fDiv = (/) `on` fromIntegral
-------------------------------------------------------------------
-- Util : averageFreq - calcurating average for counting expression
averageFreq  xs = map   (uncurry fDiv)                            $
                  scanl (\(tv, tc) (v,c) -> (tv+v*c, tc+c)) (0,0) $
                  xs
-------------------------------------------------------------------
-- Srm626 FixedDiceGameDiv2Lev2
getExpectation  a b  = last $
                       averageFreq [(x, min (x-1) b)| x <- [1,2..a]]
-------------------------------------------------------------------
main = do
    print $ getExpectation 2 2 
    print $ getExpectation 4 2 
    print $ getExpectation 3 3
    print $ getExpectation 11 13

 {-  Output
     2.0
     3.2
     2.6666666666666665
     8.0
 -}