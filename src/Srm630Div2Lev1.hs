-- Srm630Div2Lev1 DoubleLetter
-- http://community.topcoder.com/stat?c=problem_statement&pm=13378
module Srm630Div2Lev1 where
------------------------------------------------------
handler []          evt = [evt]         -- push
handler (top:stack) evt
           | top == evt = (stack)       -- pop
           | otherwise  = (evt:top:stack) -- push
------------------------------------------------------
doubleLetter xs = if [] == foldl handler [] xs then "Possible" else "Impossible"
------------------------------------------------------
main = do
    print $ doubleLetter "aabccb"
    print $ doubleLetter "aabccbb"
    print $ doubleLetter "abcddcba"
    print $ doubleLetter "abab"
    print $ doubleLetter "aaaaaaaaaa"
    print $ doubleLetter "aababbabbaba"
    print $ doubleLetter "zzxzxxzxxzzx"

{- Output
"Possible"
"Impossible"
"Possible"
"Impossible"
"Possible"
"Impossible"
"Possible"
-}
