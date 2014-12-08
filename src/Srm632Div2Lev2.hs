-- Srm632Div2Lev2 PotentialGeometricSequence
-- http://community.topcoder.com/stat?c=problem_statement&pm=13390

module Srm632Div2Lev2 where

---------------------------------------------------------
handler (diff, run) (prev, current) = (newDiff,  newRun)
  where
    newDiff = current - prev
    newRun  = if (diff == newDiff) then (run + 1) else 2

---------------------------------------------------------
numberOfSubsequences (x:xs) = sum $
                              map snd $
                              scanl handler (x,0) $
                              zip (0:x:xs) (x:xs) -- [(prev, current)]
---------------------------------------------------------

-- | The main entry point.
main :: IO ()
main = do
    print $ numberOfSubsequences [0,1,2]
    print $ numberOfSubsequences [1,2,4]
    print $ numberOfSubsequences [3,2,1,0]
    print $ numberOfSubsequences [1,2,4,8,16]
    print $ numberOfSubsequences [1,3,5,5,5,5,64,4,23,2,3,4,5,4,3]

{-| Output
6
5
10
9
37
-}