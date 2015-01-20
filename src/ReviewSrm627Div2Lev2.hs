module ReviewSrm627Div2Lev2 where

import Data.Tuple
import qualified Data.Map as M

countTable tbl key = M.insertWith (+) key 1 tbl

toHappy total num ch | div total 2 < num = ch
                     | otherwise         = '.'

getHappyLetter cs = last
    $ map   toHappy'
    $ zip   [0,1..]
    $ map   maximum
    $ map   (map swap)
    $ map   M.toList
    $ scanl countTable M.empty 
    $ cs
    where 
      toHappy' (total,(num, ch)) = toHappy total num ch 

main = do
    print $ getHappyLetter "aacaa"
    print $ getHappyLetter "dcdjx"
    print $ getHappyLetter "bcbbbbba"
    print $ getHappyLetter "aabc"
    print $ getHappyLetter "oyyyoowoooofwjoooooffyyooo"         -- o
    print $ getHappyLetter "jyjjkjjkjjykkkkyjyjkkkyjkykyjjkkjkjykkxjjk"