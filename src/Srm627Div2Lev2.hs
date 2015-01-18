-- Srm627Div2Lev2 HappyLetter
-- http://community.topcoder.com/stat?c=problem_statement&pm=13245&rd=16008
module Srm627Div2Lev2 where    
------------------------------------------------------
import qualified Data.Map as M
------------------------------------------------------
handler (maxLetter, maxCnt, table, totalCnt) letter = (maxLetter', maxCnt', table', totalCnt+1)
    where cnt'   = 1 + M.findWithDefault 0 letter table
          table' = M.insert letter cnt' table
          (maxLetter', maxCnt') = if maxCnt < cnt' then (letter, cnt') else (maxLetter, maxCnt)
          
------------------------------------------------------
happyLetter xs = last $  
    map (\(a,b,_,d)-> if (2*b > d) then a else '.') $ 
    scanl handler ('a', 0, M.empty, 0) $ 
    xs
------------------------------------------------------
main = do 
    print $ happyLetter "aacaaa"
    print $ happyLetter "dcdjx"
    print $ happyLetter "bcbbbbba"
    print $ happyLetter "aabc"