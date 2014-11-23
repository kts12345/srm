-- | Main entry point to the application.
module Main where

-------------------------------------------------------
countPeaks [] = 0
countPeaks xs = length [x| let g  = minimum xs - 1,
                           let ls = g:xs,
                           let rs = tail xs ++ [g],
                           (l,x,r) <-zip3 ls xs rs,
                             l < x && x > r]
-------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
    print $ countPeaks [5, 6, 2, 4]
    print $ countPeaks [1, 1, 1, 1, 1, 1, 1]
    print $ countPeaks [2, 1]
    print $ countPeaks [2,5,3,7,2,8,1,3,1]
    print $ countPeaks [1]
    print $ countPeaks [1,2,3,4,4,3,2,1]
    print $ countPeaks []
