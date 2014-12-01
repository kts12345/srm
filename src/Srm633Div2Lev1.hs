-- http://community.topcoder.com/stat?c=problem_statement&pm=13462

module Srm633Div2Lev1 where
    
------------------------------------------------------------------
gen c n =  "#" ++ take (n-2) (repeat c) ++ "#"

------------------------------------------------------------------
target 1 =    ["#"]

target n =    [gen '#' n                        ] -- ["#########"]
           ++ [gen ' ' n                        ] -- ["#       #"]
           ++ ["# "++ x ++" #" | x<-target (n-4)] -- ["# ..... #"]
           ++ [gen ' ' n                        ] -- ["#       #"]
           ++ [gen '#' n                        ] -- ["#########"]

------------------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
    mapM_ putStrLn $ ["n = 5"]  ++ target 5  ++ ["\n"]
    mapM_ putStrLn $ ["n = 9"]  ++ target 9  ++ ["\n"]
    mapM_ putStrLn $ ["n = 13"] ++ target 13 ++ ["\n"]
    mapM_ putStrLn $ ["n = 17"] ++ target 17 ++ ["\n"]