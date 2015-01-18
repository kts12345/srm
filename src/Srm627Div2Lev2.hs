-- Srm627Div2Lev2 HappyLetter
-- http://community.topcoder.com/stat?c=problem_statement&pm=13245&rd=16008
module Srm627Div2Lev2 where    
------------------------------------------------------
import qualified Data.Map as M
------------------------------------------------------
handler (maxLetter,  maxCnt,  table , len    ) letter =
        (maxLetter', maxCnt', table', len + 1)
    where
        (maxLetter', maxCnt') | cnt' <= maxCnt  = (maxLetter, maxCnt)
                              | otherwise       = (letter   , cnt'  )
        cnt'   = 1 + M.findWithDefault 0 letter table
        table' = M.insert letter cnt' table  
------------------------------------------------------
calcHappy (letter, cnt, _, len) = if (2*cnt) > len then letter else '.'
------------------------------------------------------
happyLetter xs = last                  $
    map   calcHappy                    $
    scanl handler ('a', 0, M.empty, 0) $
    xs
------------------------------------------------------
main = do 
    print $ happyLetter "aacaaa"
    print $ happyLetter "dcdjx"
    print $ happyLetter "bcbbbbba"
    print $ happyLetter "aabc"