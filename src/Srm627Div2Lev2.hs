-- Srm627Div2Lev2 HappyLetter
-- http://community.topcoder.com/stat?c=problem_statement&pm=13245&rd=16008
module Srm627Div2Lev2 where    
------------------------------------------------------
import qualified Data.Map as M
------------------------------------------------------
handler (maxLetter, maxCnt, table) letter
   | cnt < maxCnt = (maxLetter, maxCnt, table')
   | otherwise    = (letter,    cnt,    table')
     where
           cnt    = M.findWithDefault 0 letter table + 1
           table' = M.insert letter cnt table
------------------------------------------------------
toHappy len letter cnt | (2*cnt) > len = letter
                        | otherwise     = '.'
------------------------------------------------------
happyLetter xs = last                   $
        map   toHappy'                  $
        zip   [0,1..]                   $
        scanl handler ('a', 0, M.empty) $
        xs
    where
        toHappy' (len, (letter, cnt, _)) = toHappy len letter cnt

------------------------------------------------------
main = do 
    print $ happyLetter "aacaaa"
    print $ happyLetter "dcdjx"
    print $ happyLetter "bcbbbbba"
    print $ happyLetter "aabc"