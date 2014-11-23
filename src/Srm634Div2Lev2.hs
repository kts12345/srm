module Srm634Div2Lev2 where
    
minValue n xs = maximum [0, foldl (\s->(\e->e-n+s)) n xs]

-- | The main entry point.
main :: IO ()
main = do
    print $ minValue 5    [3,3]
    print $ minValue 100  [97]
    print $ minValue 10   [9,9,9,9,9]
    print $ minValue 7    [1,2,3]
    print $ minValue 5    [3,3,3]
