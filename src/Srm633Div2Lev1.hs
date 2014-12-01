-- http://community.topcoder.com/stat?c=problem_statement&pm=13462

module Srm633Div2Lev1 where
    
------------------------------------------------------------------
gen c n =  "#" ++ take (n-2) (repeat c) ++ "#"

------------------------------------------------------------------
accOp acc n =    [gen '#' n               ] -- ["#########"]
              ++ [gen ' ' n               ] -- ["#       #"]
              ++ ["# "++ x ++" #" | x<-acc] -- ["# .acc. #"]
              ++ [gen ' ' n               ] -- ["#       #"]
              ++ [gen '#' n               ] -- ["#########"]

target n =  foldl accOp ["#"] [x | x<-[1..n], mod x 4 == 1,  1 < x]

------------------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
    mapM_ putStrLn $ ["n = 1"]  ++ target 1  ++ ["\n"]
    mapM_ putStrLn $ ["n = 5"]  ++ target 5  ++ ["\n"]
    mapM_ putStrLn $ ["n = 9"]  ++ target 9  ++ ["\n"]
    mapM_ putStrLn $ ["n = 13"] ++ target 13 ++ ["\n"]
    mapM_ putStrLn $ ["n = 17"] ++ target 17 ++ ["\n"]