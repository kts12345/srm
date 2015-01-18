-- Srm627Div2Lev2 HappyLetter
-- http://community.topcoder.com/stat?c=problem_statement&pm=13245&rd=16008
module Srm627Div2Lev2 where    
------------------------------------------------------
import qualified Data.Map as M
------------------------------------------------------
handler (maxLetter,  maxCnt,  table , len    ) letter
   | maxCnt < cnt = (letter,     cnt,    table', len + 1)
   | otherwise    = (maxLetter,  maxCnt, table', len + 1)
    where
        cnt    = M.findWithDefault 0 letter table + 1
        table' = M.insert letter cnt table  
------------------------------------------------------
calcHappyLetter (letter, cnt, _, len) | (2*cnt) > len = letter 
                                      | otherwise      = '.'
------------------------------------------------------
happyLetter xs = last                  $
    map   calcHappyLetter              $
    scanl handler ('a', 0, M.empty, 0) $
    xs
------------------------------------------------------
main = do 
    print $ happyLetter "aacaaa"
    print $ happyLetter "dcdjx"
    print $ happyLetter "bcbbbbba"
    print $ happyLetter "aabc"