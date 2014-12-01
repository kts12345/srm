-- http://community.topcoder.com/stat?c=problem_statement&pm=13458

module Srm634Div2Lev2 where

minValue n xs = maximum [0, foldl (\acc x -> x-(n-acc)) n xs]

-- | The main entry point.
main :: IO ()
main = do
    print $ minValue 5    [3,3]
    print $ minValue 100  [97]
    print $ minValue 10   [9,9,9,9,9]
    print $ minValue 7    [1,2,3]
    print $ minValue 5    [3,3,3]
