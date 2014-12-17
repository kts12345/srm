-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where

update ds (a,b,l) =  ds
                     ++[(a,b,l)]
                     ++[(a,u,l+d)|(t,u,d)<-ds, t==b]
                     ++[(t,b,d+l)|(t,u,d)<-ds, u==a]
                     ++[(b,a,l)]
                     ++[(b,u,l+d)|(t,u,d)<-ds, t==a]
                     ++[(t,a,d+l)|(t,u,d)<-ds, u==b]
------------------------------------------------------
--egalitarianism3Easy' [] = 1
egalitarianism3Easy' ds = length $ foldl update [] ds
------------------------------------------------------
egalitarianism3Easy n as bs ls = egalitarianism3Easy' $ zip3 as bs ls
------------------------------------------------------
main = do
    print $ egalitarianism3Easy 4  [1,1,1]
                                   [2,3,4]
                                   [1,1,1]
    print $ egalitarianism3Easy 6  [1,2,3,2,3]
                                   [2,3,4,5,6]
                                   [2,1,3,2,3]
    print $ egalitarianism3Easy 10 [1,1,1,1,1,1,1,1,1]
                                   [2,3,4,5,6,7,8,9,10]
                                   [1000,1000,1000,1000,1000,1000,1000,1000,1000]
    print $ egalitarianism3Easy 2  [1]
                                   [2]
                                   [3]
    print $ length $ zip3 [] [] []
    print $ egalitarianism3Easy 1  []
                                   []
                                   []

{- Output
"Possible"
"Impossible"
"Possible"
"Impossible"
"Possible"
"Impossible"
"Possible"
-}