module SnowRefactoringVer1 where
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
close e      =  elem e  $ map  last  kPairs
-------------------------------------------------------------
f (stack, state) a
    | state == False   = (stack, state)
    | close a          = remove_pair
    | otherwise        = (push a stack, state)
    where remove_pair
            | empty stack            = (stack,     False)
            | is_pair (top stack) a  = (pop stack, True )
            | otherwise              = (stack,     False)
-------------------------------------------------------------
make_answer (stack, state)
    | ((empty stack) && (state == True)) = True
    | otherwise                          = False
-------------------------------------------------------------
make_all_cases expression =
     foldl (\ss bs -> [s++[b]| s<-ss, b<-bs]) [""]      $
     map   (\a -> if (a == 'X') then "()[]{}" else [a]) $
     expression
-------------------------------------------------------------
ifPossible expression =
    (\answer -> if answer then "possible" else "impossible") $
    any (\es-> make_answer $ last $ scanl f ([],True) es)    $
    make_all_cases expression
-------------------------------------------------------------
main :: IO ()
main = do
    print $ ifPossible "([]{})"                 -- possible
    print $ ifPossible "(())[]"                 -- possible
    print $ ifPossible "({])"                   -- impossible
    print $ ifPossible "[]X"                    -- impossible
    print $ ifPossible "([]X()[()]XX}[])X{{}}]" -- possible
