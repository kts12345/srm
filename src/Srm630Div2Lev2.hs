-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List
import Control.Arrow
import qualified Data.Map.Strict as M
------------------------------------------------------
frequency xs         = map (head &&& length) $ group $ sort xs
mergeFrequency xs ys = M.toList$ M.fromListWith (+) $ xs++ys
------------------------------------------------------
findAllNewPaths ps (a,b,l) =
       [(a,b,l)]                       -- (a,b)
    ++ [(a,u,l+d)|(t,u,d)<-ps, t==b]   -- (a,b)++(b,..u)        => (a,u)  
    ++ [(t,b,d+l)|(t,u,d)<-ps, u==a]   --        (t,..a)++(a,b) => (t,b)
    ++ [(b,u,l+d)|(t,u,d)<-ps, t==a]   -- (b,a)++(a,..u)        => (b,u)
    ++ [(t,a,d+l)|(t,u,d)<-ps, u==b]   --        (t,..b)++(b,a) => (t,a)
    ++ [(t1,u2,d1+l+d2) | (t1,u1,d1)<-ps, (t2,u2,d2)<-ps, u1==a, t2==b]
------------------------------------------------------
updateOutDistanceCounts outCnts newPaths = newOutCnts
    where
        newOutCnts  = frequency $ [(t,d)| (t,u,d)<-newPaths] ++ [(u,d)| (t,u,d)<-newPaths]
        newOutCnts' = mergeFrequency outCnts newOutCnts'
------------------------------------------------------
handler (outCnts, paths) (a,b,l) = (outCnts', paths')
    where
        newPath  = findAllNewPaths paths (a,b,l)
        paths'   = paths ++ newPath
        outCnts' = updateOutDistanceCounts outCnts newPath
------------------------------------------------------
egalitarianism3Easy n | n < 3     = (\_ _ _ ->n)
                      | otherwise = egalitarianism3Easy'
egalitarianism3Easy' xs ys lengths =
    maximum $ map snd $ fst $ foldl handler ([],[]) $ zip3 xs ys lengths
------------------------------------------------------
test  1 xs ys = 0
test  n xs ys = length $ zip xs ys
main = do
--    print $ updateCount (1,6) $ M.fromList [((2,3),6), ((1,2),4)]
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
--    print $ egalitarianism3Easy 1  [] [] []

    print $ test 1 [] []
    print $ test 3 [3,4] [6,7]


{- Output
3
3
9
2
1
-}