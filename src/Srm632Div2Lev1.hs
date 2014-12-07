-- http://community.topcoder.com/stat?c=problem_statement&pm=13391
-- RunningAroundPark

module Srm632Div2Lev1 where

handler (prev, lap) x = (x, lap + if prev < x then 0 else 1)
numberOfLap n xs = snd $ foldl handler (n,0) xs

-- | The main entry point.
main :: IO ()
main = do
        print $ numberOfLap 3  [1,2,3]
        print $ numberOfLap 24 [6,6]
        print $ numberOfLap 3  [3,2,1]
        print $ numberOfLap 50 [1,3,5,7,9,2,4,6,8,10]
