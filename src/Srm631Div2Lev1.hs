module Srm631Div2Lev1 where

import Data.List

------------------------------------------------------
handler(maxs, counts, prevs) nows = (nowMaxs, nowCounts, nows)
  where nowCounts = map update $ zip3 counts prevs nows
            where update (count, prev, now) = if prev == now then count + 1 else 1
        nowMaxs   = map max $ zip maxs nowCounts    
------------------------------------------------------
taroGrid (x:xs) = maximum $ snd $ foldl handler init xs
  where init = (replicate (length x) 0, replicate (length x) 1, x)
------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
    print $ taroGrid ["W"]    
    print $ taroGrid ["WB", "BW"]    
    print $ taroGrid ["BWW", "BBB", "BWB"]    
    print $ taroGrid ["BWBW", "BBWB", "WWWB", "BWWW"]
    print $ taroGrid ["BWB", "BBW", "BWB"] 
    print $ taroGrid ["BBWWBBWW", "BBWWBBWW", "WWBBWWBB", "WWBBWWBB", "BBWWBBWW", "BBWWBBWW", "WWBBWWBB", "WWBBWWBB"]
