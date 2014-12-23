-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
-- cluster :: (distance, nodes [node_0,node_1,..])
makeCluster  ((a,b),d) = (d, [a,b])
------------------------------------------------------
updateClusters :: [(Int, [Int])] -> [Int] -> M.Map (Int,Int) Int -> [(Int, [Int])] 
updateClusters clusters nodes pathTable = clusters'
  where addnode clusters n = map (addToCluster n) clusters
        clusters' = foldl addnode clusters nodes
        addToCluster n (distance, nodes) = cluster'
            where lookup (a,b) = M.lookup (a,b) pathTable
                  isSame n'    = elem (Just distance) [lookup (n,n'), lookup (n',n)]
                  cluster'     = (distance, nodes ++ (if all isSame nodes then [n] else []))
------------------------------------------------------
findAllNewPaths :: M.Map (Int,Int) Int -> ((Int,Int), Int) -> M.Map (Int,Int) Int 
findAllNewPaths paths ((a,b),l) = M.fromList $ 
       [((a,b),l)]                          -- (a,b             ) => (a,b)
    ++ [((a,u),l+d)|((t,u),d)<-ps, t==b]    -- (a,b=t,..u       ) => (a,u)
    ++ [((t,b),d+l)|((t,u),d)<-ps, u==a]    -- (    t,..u=a,b   ) => (t,b)
    ++ [((b,u),l+d)|((t,u),d)<-ps, t==a]    -- (b,a=t,..u       ) => (b,u)
    ++ [((t,a),d+l)|((t,u),d)<-ps, u==b]    -- (    t,..u=b,a   ) => (t,a)
    ++ [((t,u),d1+l+d2) |((t, u1),d1)<-ps,  -- (t,..u1=a,b=t2..u) => (t,u)
                         ((t2,u), d2)<-ps, u1==a, t2==b]
    where ps = M.toList paths
------------------------------------------------------
handler :: ([(Int, [Int])], M.Map (Int, Int) Int) -> (Int, Int, Int)-> ([(Int, [Int])], M.Map (Int,Int) Int)
handler (clusters, pathTable) (a,b,l) = (clusters'', pathTable')
    where
        added      = findAllNewPaths pathTable ((a,b),l)
        pathTable' = M.union pathTable added
        nodes      = nub $ foldl (\s (a,b)->s++[a,b]) [] $ map fst $ M.toList added 
        clusters'  = updateClusters clusters nodes pathTable'
        clusters'' = clusters' ++ (map makeCluster $ M.toList added)
------------------------------------------------------
egalitarianism3Easy n xs ys lengths
    | n <= 2    = n
    | otherwise = maximum    $
                  map length $
                  map snd    $
                  fst $
                  foldl handler ([], M.empty) $ zip3 xs ys lengths 
------------------------------------------------------
main = do
    print $ egalitarianism3Easy 4  [1,1,1]
                                  [2,3,4]
                                  [1,1,1]

    print $ egalitarianism3Easy 6  [1,2,4,3,3]
                                   [2,5,3,2,6]
                                   [2,2,3,1,3]
    print $ egalitarianism3Easy 10 [1,1,1,1,1,1,1,1,1]
                                   [2,3,4,5,6,7,8,9,10]
                                  [1000,1000,1000,1000,1000,1000,1000,1000,1000]
    print $ egalitarianism3Easy 2  [1]
                                  [2]
                                  [3]
    print $ egalitarianism3Easy 1  [] [] []


{-  Output
3
3
9
2
1
-}