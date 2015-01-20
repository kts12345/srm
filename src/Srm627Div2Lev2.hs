-- Srm627Div2Lev2 HappyLetterDiv2
-- http://community.topcoder.com/stat?c=problem_statement&pm=13245&rd=16008
module Srm627Div2Lev2 where
------------------------------------------------------
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple
------------------------------------------------------
handler (tbl, (numMx, chMx)) ch = (tbl', max (numMx, chMx) (num, ch)) 
    where 
        (old, tbl') = M.insertLookupWithKey (\_ _ o -> o+1) ch 1 tbl
        num | isJust old = 1 + fromJust old
            | otherwise  = 1
------------------------------------------------------
toHappy total num ch | total < (2*num) = ch
                     | otherwise       = '.'
------------------------------------------------------
happyLetter''' xs = 
    last                             $
    map   toHappy'                   $
    zip   [0,1..]                    $
    scanl handler (M.empty, (0,'a')) $
    xs 
    where toHappy' (total, (_, (num, ch))) = toHappy total num ch
------------------------------------------------------
main = do 
    print $ happyLetter "aacaaa"
    print $ happyLetter "dcdjx"
    print $ happyLetter "bcbbbbba"
    print $ happyLetter "aabc"
{- Output
 'a'
 '.'
 'b'
 '.'
-}
------------------------------------------------------
-- if you need more simple code for batch-job.
happyLetter xs =
    (\(n,c)-> if 2*n < length xs then '.' else c) $
    maximum                                       $
    map swap                                      $
    M.toList                                      $
    M.fromListWith (+)                            $
    zip xs [1,1..]
------ or ------
happyLetter' xs =
    (\m->if m == M.empty then '.' else fst (M.elemAt 0 m)) $
    M.filter ( > div (length xs) 2)                        $
    M.fromListWith (+)                                     $
    zip xs [1,1..]
-- end of batch-job ----------------------------------