-- Srm626Div2Lev1 SumOfPower
-- http://community.topcoder.com/stat?c=problem_statement&pm=13230
module Srm626Div2Lev1 where
------------------------------------------------------
sumOfPower :: [Int] -> Int
sumOfPower xs = last              $
                scanl1 (+)        $
                scanl1 (+)        $
                map (uncurry (*)) $
                zip [1..]       $
                xs
------------------------------------------------------
main :: IO ()
main = do
    print $ sumOfPower [1,2]
    print $ sumOfPower [1,1,1] 
    print $ sumOfPower [3,14,15,92,65]
    print $ sumOfPower [1,2,3,4,5,6,7,8,9,10]
 {-  Output
     6
     10
     1323
     1210
 -}
------------------------------------------------------
-- if you need another code for batch-job. O(n)
sumOfPower' :: [Int] -> Int
sumOfPower' xs = sum.map (\(a,b,c)->a*b*c) $
                 zip3  [1..n] [n,(n-1)..1] $
                 xs
       where n = length xs