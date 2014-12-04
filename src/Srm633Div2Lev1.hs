-- http://community.topcoder.com/stat?c=problem_statement&pm=13462

module Srm633Div2Lev1 where

accOp acc x = ["##" ++ fill '#' ++ "##"]           -- ["#########"]
           ++ ["# " ++ fill ' ' ++ " #"]           -- ["#       #"]
           ++ ["# " ++   a      ++ " #"| a <- acc] -- ["# .acc. #"]
           ++ ["# " ++ fill ' ' ++ " #"]           -- ["#       #"]
           ++ ["##" ++ fill '#' ++ "##"]           -- ["#########"]
           where fill e = [e|_<-[1..x-4]]          -- replicate (x-4) e


target n =  foldl accOp ["#"] [5,5+4..n]
 
------------------------------------------------------------------
-- | The main entry point.
main :: IO ()
main = do
    mapM_ putStrLn $ ["n = 1"]  ++ target 1  ++ ["\n"]
    mapM_ putStrLn $ ["n = 5"]  ++ target 5  ++ ["\n"]
    mapM_ putStrLn $ ["n = 9"]  ++ target 9  ++ ["\n"]
    mapM_ putStrLn $ ["n = 13"] ++ target 13 ++ ["\n"]
    mapM_ putStrLn $ ["n = 17"] ++ target 17 ++ ["\n"]
{-|

출력값.

n = 1
#


n = 5
#####
#   #
# # #
#   #
#####


n = 9
#########
#       #
# ##### #
# #   # #
# # # # #
# #   # #
# ##### #
#       #
#########


n = 13
#############
#           #
# ######### #
# #       # #
# # ##### # #
# # #   # # #
# # # # # # #
# # #   # # #
# # ##### # #
# #       # #
# ######### #
#           #
#############


n = 17
#################
#               #
# ############# #
# #           # #
# # ######### # #
# # #       # # #
# # # ##### # # #
# # # #   # # # #
# # # # # # # # #
# # # #   # # # #
# # # ##### # # #
# # #       # # #
# # ######### # #
# #           # #
# ############# #
#               #
#################

-}

-------------------------------------------------------------------------------------
-- pudae code review
-------------------------------------------------------------------------------------
draw n = [row x | x <-[0..n-1]]
	where
		toDistance (x,y) = max (abs (x-c)) (abs (y-c)) where c = div (n-1) 2
		toChar d         = if even d then '#' else ' '
		row    x         = map toChar $ map toDistance [(x,y)|y<-[0..n-1]]
-------------------------------------------------------------------------------------
buffer count step [] = []
buffer count step xs = take count xs : buffer count step (drop step xs)
draw2 n = buffer n n cs
	where
		toDistance (x,y) = max (abs (x-c)) (abs (y-c)) where c = div (n-1) 2
		toChar d         =  if even d then '#' else ' '
		cs               = map toChar $ map toDistance [(x,y) | y<-[0..n-1], x<-[0..n-1]]
-------------------------------------------------------------------------------------
