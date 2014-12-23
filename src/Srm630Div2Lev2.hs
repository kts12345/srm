-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List
import Data.Maybe
import Control.Arrow
-- import Debug.Trace
import qualified Data.Map.Strict as M

trace a b = b
------------------------------------------------------
allM f xs = all isJust xs  
            &&
            all (\e-> f (fromJust e)) xs 
------------------------------------------------------
-- cluster :: (distance, set [node_0,node_1,..]) 
makeCluster :: ((Int,Int),Int)->(Int,[Int])
makeCluster  ((a,b),d) = (d, [a,b])
------------------------------------------------------
lookDistance :: Int -> Int -> M.Map (Int,Int) Int -> Maybe Int 
lookDistance a b pathTable 
  | isJust (distance a b) = distance a b 
  | otherwise             = distance b a 
      where distance t u    = M.lookup (t,u) pathTable
------------------------------------------------------
addToCluster :: (Int,[Int]) -> Int -> M.Map (Int,Int) Int -> (Int, [Int])
addToCluster (distance, nodes) node  pathTable = trace ("addToCluster : cluster="++ show (distance,nodes) ++ " node="++ show node ++" allD=" ++ show allD) (distance, nodes') 
  where
    findDistance node' = trace ("findDistance: (node,node')="++ (show (node,node'))++", distance=" ++ (show distance)++ ", result=" ++ show (lookDistance node node' pathTable) ++ " pathTable=" ++ show pathTable) lookDistance node node' pathTable 
    allD     = allM  (\d-> d == distance) $ map findDistance nodes
    nodes'   = nodes ++ (if allD then [node] else [])
------------------------------------------------------
updateClusters :: [(Int, [Int])] -> [Int] -> M.Map (Int,Int) Int -> [(Int, [Int])] 
updateClusters clusters nodes pathTable = clusters' 
  where add :: Int->(Int,[Int])->(Int,[Int])
        add node cluster = addToCluster cluster node pathTable 
        handler :: [(Int, [Int])]->Int->[(Int, [Int])]
        handler clusters a = map (add a) clusters
        clusters' = foldl handler clusters nodes
------------------------------------------------------
findAllNewPaths :: M.Map (Int,Int) Int -> ((Int,Int), Int) -> M.Map (Int,Int) Int 
findAllNewPaths paths ((a,b),l) = trace ("findAllNewPaths" ++ show ((a,b),l)) M.fromList $ 
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
handler (clusters, pathTable) (a,b,l) = trace ("main handler1. (a,b,l)=" ++ show (a,b,l) ++ " clusters''=" ++ show clusters'') (clusters'', pathTable')
    where
        added      = trace ("added (a,b,l)=" ++ show (a,b,l)) findAllNewPaths pathTable ((a,b),l)
        pathTable' = trace ("M.union (a,b,l)=" ++ show (a,b,l)) M.union pathTable added
        nodes      = trace ("nodes (a,b,l)=" ++ show (a,b,l)) nub $ foldl (\s (a,b)->s++[a,b]) [] $ map fst $ M.toList added 
        clusters'  = trace ("clusters'" ++ show (a,b,l) ++ " nodes=" ++ show nodes) updateClusters clusters nodes pathTable'
        clusters'' = clusters' ++ (map makeCluster $ M.toList added)
------------------------------------------------------
-- egalitarianism3Easy :: Int->[Int]->[Int]->[Int]->Int
egalitarianism3Easy n xs ys lengths
    | n <= 2    = n
    | otherwise = maximum    $
                  map length $
                  map snd    $
                  fst $
                  foldl handler ([], M.empty) $ zip3 xs ys lengths 
------------------------------------------------------
m = M.fromList[((2,3),4), ((1,2),4)]
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


{- Expected Output
3
3
9
2
1
-}