-- http://community.topcoder.com/stat?c=problem_statement&pm=13390&rd=16075
-- PotentialGeometricSequence
module Srm632Div2Lev2 where
 
--numberOfSubsequences xs = repeat 2 3
handler (diff,count) (prev, x) = (x - prev, if -prev == diff then count+3 else count + 2)
toDiff (prev,x) = x-prev
toCount (prev, x) = if prev == x then 3 else 2
numberOfSubsequences xs = sum counts
    where
       counts = map toCount zip(diffs)

     --  {1 ,    2,     4,     8,     16 }
     --  {   (1,2), (2,4), (4,8), (8,16) } <= zip
     --  {       1,     2,     4,      8 } <= toDiff
     --  {          (1,2),  (2,4), (4,8) } <= zip
     --  { #1   #2,     2       2      2 } <= toCount
     
-- | The main entry point.
main :: IO ()
main = do
    print $ 4