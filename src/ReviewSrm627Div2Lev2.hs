module ReviewSrm627Div2Lev2 where

import Debug.Trace
import Data.List
import Data.Tuple
import qualified Data.Map as M

updateMap m l = M.insertWith (+) l 1 m

updateState (count, m) c = (count + 1, updateMap m c)

compareSwap v1 v2 = compare (swap v1) (swap v2)

toHappyLetter (total, (c, count)) | div total 2 < count = c
                                  | otherwise           = '.'

getHappyLetter cs = 
          last
        $ map toHappyLetter
        $ map (\(count, ls) -> (count, maximumBy compareSwap ls))
        $ map (\(count, m)  -> (count, M.toList m))
        $ tail
        $ scanl updateState (0, M.empty) cs

main = do
    print $ getHappyLetter "aacaa"
    print $ getHappyLetter "dcdjx"
    print $ getHappyLetter "bcbbbbba"
    print $ getHappyLetter "aabc"
    print $ getHappyLetter "oyyyoowoooofwjoooooffyyooo"         -- o    
    print $ getHappyLetter "jyjjkjjkjjykkkkyjyjkkkyjkykyjjkkjkjykkxjjk"