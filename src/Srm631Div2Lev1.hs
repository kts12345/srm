-- Srm631Div2Lev1 TaroGrid
-- http://community.topcoder.com/stat?c=problem_statement&pm=13394
module Srm631Div2Lev1 where
------------------------------------------------------
handler (cnts,maxs,prevs) nows = (cnts',maxs',nows)
   where cnts' = zipWith3 (\p n c -> if p==n then c+1 else 1) prevs nows cnts
         maxs' = zipWith max cnts' maxs
------------------------------------------------------
taroGrid xs = maximum $
             (\(_,m,_)->m) $
             foldl handler ([0,0..],[0,0..],['X','X'..]) xs
------------------------------------------------------
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