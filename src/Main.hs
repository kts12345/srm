sValue n xs = maximum [0, foldl (\s->(\e->e-n+s)) n xs]


-- | The main entry point.
main :: IO ()
main = do
    print $ sValue 5    [3,3]
    print $ sValue 100  [97]
    print $ sValue 10   [9,9,9,9,9]
    print $ sValue 7    [1,2,3]
    print $ sValue 5    [3,3,3]
   