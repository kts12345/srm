-- Srm628Div2Lev2 BracketExpressions
-- http://community.topcoder.com/stat?c=problem_statement&pm=13243
module Srm628Div2Lev2 where
------------------------------------------------------
kBrackets = ["()","[]","{}"]
stringAnswer True  = "Possible"
stringAnswer False = "Impossible"
------------------------------------------------------
list 'X' = foldl1 (++) kBrackets -- "()[]{}"
list e   = [e]
------------------------------------------------------
handler stacks es = stacks'
    where stacks' = [check e stack | stack<-stacks, e<-es]
          check e []                       = [e]          -- push
          check e (top:tail)| match top e  = tail         -- pop
                            | otherwise    = (e:top:tail) -- push
          match e1 e2 = elem [e1,e2] kBrackets
------------------------------------------------------
bracketExpressions xs =  last               $
                         map   stringAnswer $
                         map   (elem [])    $
                         scanl handler [""] $
                         map   list         $
                         xs
------------------------------------------------------
main = do
 print $ bracketExpressions  "([]{})"
 print $ bracketExpressions  "(())[]"
 print $ bracketExpressions  "({])"
 print $ bracketExpressions  "[]X"
 print $ bracketExpressions  "([]X()[()]XX}[])X{{}}]"
------------------------------------------------------
{- Output

 Possible
 Possible
 Impossible
 Impossible
 Possible

-}