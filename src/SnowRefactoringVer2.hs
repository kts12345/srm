module SnowRefactoringVer2 where
-------------------------------------------------------------
-- Util : Stack
type Stack a = [a]
push :: a -> Stack a -> Stack a
push x xs = x:xs

pop :: Stack a -> Stack a
pop (x:xs) = xs

top :: Stack a -> a
top (x:xs) = x

empty :: Stack a -> Bool
empty [] = True
empty (x:xs) = False
-------------------------------------------------------------
kPairs       =  ["()","[]","{}"]
is_pair a b  =  elem [a,b] kPairs
open e       =  elem e  $ map  head kPairs
-------------------------------------------------------------
f stacks a = [snd res | stack <-stacks, let res = f' stack, fst res]
    where f' stack  | open a                 = (True,  push a stack)
                    | empty stack            = (False, [])
                    | is_pair (top stack) a  = (True,  pop stack)
                    | otherwise              = (False, [])
-------------------------------------------------------------
make_answer []      = False
make_answer [stack] = empty stack
-------------------------------------------------------------
make_all_cases expression =
     foldl (\ss bs -> [s++[b]| s<-ss, b<-bs]) [""]      $
     map   (\a -> if (a == 'X') then "()[]{}" else [a]) $
     expression
-------------------------------------------------------------
ifPossible expression =
    (\answer -> if answer then "possible" else "impossible") $
    any (\es -> make_answer $ last $ scanl f [""] es)        $
    make_all_cases expression
-------------------------------------------------------------
main :: IO ()
main = do
    print $ ifPossible "([]{})" -- possible
    print $ ifPossible "(())[]" -- possible
    print $ ifPossible "({])"  -- impossible
    print $ ifPossible "[]X" -- impossible
    print $ ifPossible "([]X()[()]XX}[])X{{}}]" -- possible