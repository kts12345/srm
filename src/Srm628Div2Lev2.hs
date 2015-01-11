-- Srm628Div2Lev2 BracketExpressions
-- http://community.topcoder.com/stat?c=problem_statement&pm=13243
module Srm628Div2Lev2 where
------------------------------------------------------
kPairs   = ["()","[]","{}"]
pair a b = elem [a,b] kPairs
stringAnswer True  = "Possible"
stringAnswer False = "Impossible"
------------------------------------------------------
list 'X' = foldl1 (++) kPairs -- "()[]{}"
list e   = [e]
------------------------------------------------------
handler stacks es = stacks'
    where stacks' = [check e stack | stack<-stacks, e<-es]
          check e []                     = [e]          -- push
          check e (top:tail)| pair top e = tail         -- pop
                            | otherwise  = (e:top:tail) -- push
------------------------------------------------------
bracketExpressions xs =  last               $   -- 6.                                                 "ImPossible"
                         map   stringAnswer $   -- 5. [ "Imossible",  "Possible"                   ,  "Impossible"                       ]
                         map   (elem [])    $   -- 4. [  False     ,   True                        ,   False                             ]
                         scanl handler [""] $   -- 3. [ ["("]      ,  ["((","","([","(]","({","(}"],  ["((}","}","([}","(]}","(" ,"(}}"] ]
                         map   list         $   -- 2. [ "("        ,  "()[]{}"                     ,  "}"                                ]
                         xs                     -- 1. [ '('        ,  'X'                          ,  '}'                                ]
                                                -- 0. ex) xs == "(X}"
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