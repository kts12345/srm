-- Srm628Div2Lev2 BracketExpressions
-- http://community.topcoder.com/stat?c=problem_statement&pm=13243
module Srm628Div2Lev2 where
------------------------------------------------------
kBrackets = "()[]{}"
match e1 e2 | [e1,e2] == "()"  = True
            | [e1,e2] == "[]"  = True
            | [e1,e2] == "{}"  = True
            | otherwise        = False
stringAnswer True  = "Possible"
stringAnswer False = "Impossible"
------------------------------------------------------
gen 'X' = kBrackets
gen e   = [e]
------------------------------------------------------
apply stacks es = stacks'
    where stacks' = [check e stack | stack<-stacks, e<-es]
          check e []                       = [e]          -- push
          check e (top:tail)| match top e  = tail         -- pop
                            | otherwise    = (e:top:tail) -- push 
------------------------------------------------------
bracketExpressions xs =  last               $
                         map   stringAnswer $
                         map   (elem [])    $
                         scanl apply [[]]   $
                         map   gen          $
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