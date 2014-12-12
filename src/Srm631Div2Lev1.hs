module Srm631Div2Lev1 where

import Data.List

------------------------------------------------------
handler(counts, prevs) nows = (map update $ zip3 counts prevs nows, nows)
  where update(count, prev, now) = if prev == now then count + 1 else 1 
------------------------------------------------------
taroGrid (x:xs) = maximum $ fst $ foldl handler init xs
  where init = (replicate (length x) 1, x)
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
