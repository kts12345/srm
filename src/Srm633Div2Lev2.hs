-- http://community.topcoder.com/stat?c=problem_statement&pm=13463
module Srm633Div2Lev2 where
    
jumpingAble x y xs = if able then "Able" else "Not able" 
  where 
       ls   = xs ++ [sqrt (x*x + y*y)]
       (sum', max') = foldl (\(s,m) x->(s+x, max m x)) (0.0, 0.0) ls
       able = max' <= sum'-max'

-----------------------------------------------------------------------    
-- | The main entry point.
main :: IO ()
main = do
  print $ jumpingAble 5 4 [2, 5] -- "Able"
  print $ jumpingAble 3 4 [4 ]   -- "Not Able"
  print $ jumpingAble 3 4 [6 ]   -- "Not Able"
  print $ jumpingAble 0 1 [100 , 100] -- "Able"
  print $ jumpingAble 300 400 [500]  -- "Able"
  print $ jumpingAble 11 12 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] -- "Able"
  print $ jumpingAble 11 12 [1, 2, 3, 4, 5, 6, 7, 8, 9, 100] -- "Not Able"