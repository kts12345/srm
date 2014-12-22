-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
------------------------------------------------------
frequency :: [(Int,Int)]->[((Int,Int),Int)]
frequency xs         = map (head &&& length) $ group $ sort xs
------------------------------------------------------
-- mergeFrequency :: [((Int,Int),Int)]->[((Int,Int),Int)]->[((Int,Int),Int)]
mergeFrequency xs ys = M.toList$ M.fromListWith (+) $ xs++ys
------------------------------------------------------
findAllNewPaths ps (a,b,l) =
       [(a,b,l)]                        -- (a,b             ) => (a,b)
    ++ [(a,u,l+d)|(t,u,d)<-ps, t==b]    -- (a,b=t,..u       ) => (a,u)
    ++ [(t,b,d+l)|(t,u,d)<-ps, u==a]    -- (    t,..u=a,b   ) => (t,b)
    ++ [(b,u,l+d)|(t,u,d)<-ps, t==a]    -- (b,a=t,..u       ) => (b,u)
    ++ [(t,a,d+l)|(t,u,d)<-ps, u==b]    -- (    t,..u=b,a   ) => (t,a)
    ++ [(t,u,d1+l+d2) | (t, u1,d1)<-ps, -- (t,..u1=a,b=t2..u) => (t,u)
                        (t2,u, d2)<-ps, u1==a, t2==b]
------------------------------------------------------
-- updateOutDistanceCounts ::[((Int,Int),Int)]->[(Int,Int,Int)]->[((Int,Int),Int)]
updateOutDistanceCounts outCnts newPaths = newOutCnts'
    where
        newOutCnts  = frequency $ [(t,d)| (t,u,d)<-newPaths] ++ [(u,d)| (t,u,d)<-newPaths]
        newOutCnts' = mergeFrequency outCnts newOutCnts
------------------------------------------------------
-- handler :: ([((Int,Int),Int)],[(Int,Int,Int)])->(Int,Int,Int)->([((Int,Int),Int)],[(Int,Int,Int)])
handler (outCnts, paths) (a,b,l) = (outCnts', paths')
    where
        newPath  = findAllNewPaths paths (a,b,l)
        paths'   = paths ++ newPath
        outCnts' = updateOutDistanceCounts outCnts newPath
------------------------------------------------------
-- egalitarianism3Easy :: Int->[Int]->[Int]->[Int]->Int
egalitarianism3Easy n xs ys lengths
    | n <= 2    = [(1,1,1)] --  n
    | otherwise = snd $ foldl handler ([],[]) $ zip3 xs ys lengths -- maximum $ map snd $ fst $ foldl handler ([],[]) $ zip3 xs ys lengths
------------------------------------------------------
main = do
    print $ egalitarianism3Easy 4  [1,1,1]
                                   [2,3,4]
                                   [1,1,1]
    print $ "-------------------------------------"
    print $ egalitarianism3Easy 6  [1,2,4,3,3]
                                   [2,5,3,2,6]
                                   [2,2,3,1,3]
    print $ "-------------------------------------"
    print $ egalitarianism3Easy 10 [1,1,1,1,1,1,1,1,1]
                                   [2,3,4,5,6,7,8,9,10]
                                   [1000,1000,1000,1000,1000,1000,1000,1000,1000]
    print $ "-------------------------------------"
    print $ egalitarianism3Easy 2  [1]
                                   [2]
                                   [3]
    print $ egalitarianism3Easy 1  [] [] []



{- Output
3
3
9
2
1
-}