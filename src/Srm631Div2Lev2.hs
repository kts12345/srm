module Srm631Div2Lev2 where
    
import Data.List
import Control.Monad

------------------------------------------------------
handlerS (time, start) (position, count) = timeStart 
    where start'    = max start (position - time)
          end       = start' + count - 1
          endLimit  = position + time
          timeStart = if end <= endLimit then Just (time, end+1)
                                         else Nothing 

------------------------------------------------------
handler (time,xs) (p,c) = next 
        where xs'   = sort $ xs ++ [(p,c)] 
              check = foldM handlerS (time, -2001) xs'
              next  = if check == Nothing then Nothing
                                          else Just (time, xs') 

------------------------------------------------------
catsOnTheLine ps cs time = 
    if result == Nothing then "Impossible" else "Possible"
    where result = foldM handler (time, []) $ zip ps cs

------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
        print $ catsOnTheLine [0] [7] 3
        print $ catsOnTheLine [0] [8] 2
        print $ catsOnTheLine [0,1] [3,1] 0
        print $ catsOnTheLine [5, 0, 2] [2, 3, 5] 2
        print $ catsOnTheLine [5, 1, -10, 7, 12, 2, 10, 20] [3, 4,   2, 7,  1, 4,  3,  4] 6

