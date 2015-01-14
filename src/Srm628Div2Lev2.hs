-- Srm628Div2Lev2 BracketExpressions
-- http://community.topcoder.com/stat?c=problem_statement&pm=13243
module Srm628Div2Lev2 where
import Data.List
------------------------------------------------------
-- Util : Stack
type Stack a = [a]
kEmpty       = []
empty        = null
push stack a = (a:stack)
pop  stack   = tail stack
top  stack   = head stack
------------------------------------------------------
-- BracketExpressions problem Constraints
kPairs       =  ["()","[]","{}"]
pair a b     =  elem [a,b] kPairs
kOpens       =  map  head  kPairs -- "([{"
open e       =  elem e     kOpens
answer True  = "Possible"
answer False = "Impossible"
------------------------------------------------------
list 'X' = concat kPairs -- "()[]{}"
list e   = [e]
------------------------------------------------------
handler:: [Stack Char] -> [Char]-> [Stack Char]
handler stacks es = stacks'
    where
    stacks' = -- nub           $
              map    update $
              filter valid  $
              [(s, e) | s<-stacks, e<-es]
    valid  (stack, e) | open  e             =  True
                      | empty stack         =  False
                      | pair  (top stack) e =  True
                      | otherwise           =  False
    update (stack, e) | open e    =  push stack e
                      | otherwise =  pop  stack
------------------------------------------------------
bracketExpressions xs =
    last                   $ -- 6.                                            "ImPossible"
    map   answer           $ -- 5. [ "Imossible",   "Possible"              , "Impossible" ]
    map   (any empty)      $ -- 4. [ False      ,    True                   ,  False       ]
    scanl handler [kEmpty] $ -- 3. [ [ "(" ]    ,  [ "((", "", "([", "({" ] ,  [ "(" ]     ]
    map   list             $ -- 2. [   "("      ,    "()[]{}"               ,    "}"       ]
    xs                       -- 1. [   '('      ,    'X'                    ,    '}'       ]
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

{- optimization effects

  # of      | before | after  | after
   X        |        | filter | nub
--------------------------------------
            | always | worst  | worst
--------------------------------------
"..X.."     : 6     -> 4      -> 4
"..XX.."    : 36    -> 16     -> 13
"..XXX.."   : 196   -> 64     -> 40
"..XXXX.."  : 1296  -> 256    -> 121
"..XXXXX.." : 7776  -> 1024   -> 361
-}