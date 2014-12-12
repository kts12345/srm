module Srm631Div2Lev1 where

------------------------------------------------------
handler (counts, maxs) (prevs, nows) = (counts',maxs')
  where counts' = zipWith3 (\c p n -> if p ==n then c+1 else 1) counts prevs nows
        maxs'   = zipWith max maxs counts'
------------------------------------------------------
taroGrid (x:xs) =
    maximum $ snd $ foldl handler init $ zip (x:x:xs) (x:xs) 
     -- where init = ((replicate (length x) 1), (replicate (length x) 0), x)
          where init = ([0,0..], [0,0..])
------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
  --   print $ (\(_,s,_)-> s) ([1,1..], [0,0..], "B")
  --  print $ taroGrid ["W"]  
    print $ take 4 "UU.."
    print $ taroGrid ["WB", "BW"]    
    print $ taroGrid ["BWW", "BBB", "BWB"]    
    print $ taroGrid ["BWBW", "BBWB", "WWWB", "BWWW"]
    print $ taroGrid ["BWB", "BBW", "BWB"] 
    print $ taroGrid ["BBWWBBWW", "BBWWBBWW", "WWBBWWBB", "WWBBWWBB", "BBWWBBWW", "BBWWBBWW", "WWBBWWBB", "WWBBWWBB"]
