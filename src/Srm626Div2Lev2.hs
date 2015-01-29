-- Srm626Div2Lev2 FixedDiceGameDiv2
-- http://community.topcoder.com/stat?c=problem_statement&pm=13240&rd=15859
module Srm626Div2Lev2 where
-------------------------------------------------------------------
import Data.Function
-------------------------------------------------------------------
-- Util : Integral Division
fDiv :: Integer -> Integer -> Double
fDiv = (/) `on` fromIntegral
-------------------------------------------------------------------
-- Util : sum of arithmetic sequence
sumOfNSquare :: Integral a => a -> a
sumOfNSquare n = quot (n*(n+1)*(2*n+1)) 6
sumOfN :: Integral a => a -> a
sumOfN  n = quot (n*(n+1)) 2 
-------------------------------------------------------------------
-- Srm626 FixedDiceGameDiv2Lev2
fixedDiceGame :: Integer -> Integer -> Double
fixedDiceGame a b  = fDiv sum count
  where m     = min a b
        diff  = a - m
        sum   = (sumOfNSquare m - sumOfN m) + diff*(sumOfN a - sumOfN m)
        count = sumOfN (m-1)                + diff*b
-------------------------------------------------------------------
main :: IO ()
main = do
    print $ fixedDiceGame 2 2 
    print $ fixedDiceGame 4 2 
    print $ fixedDiceGame 3 3
    print $ fixedDiceGame 11 13

 {-  Output
     2.0
     3.2
     2.6666666666666665
     8.0
 -}