-- Srm632Div2Lev1 TaroGrid
-- http://community.topcoder.com/stat?c=problem_statement&pm=13394

module Srm631Div2Lev1 where

------------------------------------------------------
handler (cnts,maxs) (prvs,nows) = (cnts', zipWith max cnts' maxs)
   where cnts' = zipWith3 (\p n c-> if p==n then c+1 else 1) prvs nows cnts
------------------------------------------------------
taroGrid (x:xs) = maximum $ snd $ foldl handler ([0,0..],[0,0..]) $ zip (x:x:xs) (x:xs)
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

{-| Output
1
1
3
3
3
2
-}