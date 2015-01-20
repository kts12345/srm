-- Srm627Div2Lev2 HappyLetter
-- http://community.topcoder.com/stat?c=problem_statement&pm=13245&rd=16008
module Srm627Div2Lev2 where
------------------------------------------------------
import qualified Data.Map as M
import Data.Maybe
------------------------------------------------------
handler (maxLetter, maxCnt, table) letter
                    | cnt < maxCnt = (maxLetter, maxCnt, table')
                    | otherwise    = (letter,    cnt,    table')
    where
        (old, table') = M.insertLookupWithKey (\_ _ v -> v+1) letter 1 table
        cnt           = 1 + (if old == Nothing then 0 else fromJust old)
------------------------------------------------------
toHappy total letter cnt | (2*cnt) > total = letter
                         | otherwise       = '.'
------------------------------------------------------
happyLetter xs = last                   $
        map   toHappy'                  $
        zip   [0,1..]                   $
        scanl handler ('a', 0, M.empty) $
        xs
    where
        toHappy' (total, (letter, cnt, _)) = toHappy total letter cnt -- syntax adaptor

------------------------------------------------------
main = do 
    print $ happyLetter "aacaaa"
    print $ happyLetter "dcdjx"
    print $ happyLetter "bcbbbbba"
    print $ happyLetter "aabc"