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
          check e []                    = [e]          -- push
          check e (top:tail)| match     = tail         -- pop
                            | otherwise = (e:top:tail) -- push
             where match = elem [top,e] kBrackets
------------------------------------------------------
bracketExpressions xs =  last               $   -- 6. "Possible"
                         map   stringAnswer $   -- 5. ["Imossible","Possible","Impossible"      , "Possible"                   ]
                         map   (elem [])    $   -- 4. [False,  True,  False                     ,  True                        ]
                         scanl handler [""] $   -- 3. [["("],  [""],  ["(","")","[","]","{","}"],  ["(}",")}","[}","[}","","}}"]
                         map   list         $   -- 2. ["("  ,  ")" ,  "()[]{}"                  ,  "}"                         ]
                         xs                     -- 1. ['('  ,  ')' ,  'X'                       ,  '}'                         ]
                                                -- 0. ex) xs == "()X}"
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

{-
([]{})
(())[]
({])
([]X()[()]XX}[])X{{}}]
([]{()[()]()}[])[{{}}]
   X      XX}   X    ]
({({{{(([[[[({})]]]]))}}})})
({({{{(([[[[({}{})]]]]))}}})})
-}
