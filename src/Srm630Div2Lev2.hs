-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where

------------------------------------------------------
egalitarianism3Easy n as bs lengths = 3
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