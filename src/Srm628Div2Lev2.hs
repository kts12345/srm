-- Srm628Div2Lev2 BracketExpressions
-- http://community.topcoder.com/stat?c=problem_statement&pm=13243
module Srm628Div2Lev2 where
------------------------------------------------------
kPairs   = ["()","[]","{}"]
pair a b = elem [a,b] kPairs
kOpens   = map head kPairs -- "([{"
open e   = elem e kOpens
stringAnswer True  = "Possible"
stringAnswer False = "Impossible"
------------------------------------------------------
list 'X' = foldl1 (++) kPairs -- "()[]{}"
list e   = [e]
------------------------------------------------------
handler stacks es = stacks'
    where stacks' = filter valid $
                    [update e stack | stack<-stacks, e<-es]
          update e []                     = [e]          -- push
          update e (top:tail)| pair top e = tail         -- pop
                             | otherwise  = (e:top:tail) -- push
          valid (top:tail)  = open top -- insight : valid stack must have only open brackets
          valid _           = True
------------------------------------------------------
bracketExpressions xs =  last               $   -- 6.                                            "ImPossible"
                         map   stringAnswer $   -- 5. [ "Imossible",   "Possible"              , "Impossible" ]
                         map   (elem "")    $   -- 4. [ False      ,    True                   ,  False       ]
                         scanl handler [""] $   -- 3. [ [ "(" ]    ,  [ "((", "", "([", "({" ] ,  [ "(" ]     ]
                         map   list         $   -- 2. [   "("      ,    "()[]{}"               ,    "}"       ]
                         xs                     -- 1. [   '('      ,    'X'                    ,    '}'       ]
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

{- optimization      
  # of      | before | after  
   X        | always | worst
-----------------------------
"..X.."     : 6     -> 3
"..XX.."    : 36    -> 12
"..XXX.."   : 196   -> 45
"..XXXX.."  : 1296  -> 180
"..XXXXX.." : 7776  -> 702
-}