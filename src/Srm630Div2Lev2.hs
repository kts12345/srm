-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List                       -- for nub
import Data.Maybe                      -- for Just
import Control.Monad                   -- for join
import qualified Data.Map.Strict as M  -- fromList, toList, union
------------------------------------------------------
-- path     :: ((node1, node2), length)
-- table    :: Map Key(node1, node2) Value(length)
-- cluster  :: (distance, nodes[node1, node2, ...])
-- clusters :: [cluster]
------------------------------------------------------
updateClusters :: [(Int, [Int])] -> [Int] -> M.Map (Int,Int) Int -> [(Int, [Int])] 
updateClusters clusters nodes table = newClusters
  where newClusters = foldl expandClusters clusters nodes
        expandClusters clusters' n     = map (expandCluster n) clusters'
        expandCluster  n (dist, nodes) = (dist, nodes ++ expandNode)
          where lookup (a,b) = M.lookup (a,b) table
                sameDist n'    = elem (Just dist) [lookup (n,n'), lookup (n',n)]
                expandNode   = if all sameDist nodes then [n] else []
------------------------------- -----------------------
findNewPaths :: M.Map (Int,Int) Int -> ((Int,Int), Int) -> [((Int,Int), Int)]
findNewPaths table path = newPaths
  where ((a,b),l) = path
        newPaths  = [((a,b),l)] ++ add (a,b) ++ add (b,a)
        add (a,b) = [((a,u),l+d)|((t,u),d)<-ps, t==b]      -- (a,    b=t,..u       ) => (a,u  )
                 ++ [((t,b),d+l)|((t,u),d)<-ps, u==a]      -- (        t,..u=a,  b ) => (t,b  )
                 ++ [((t1,u2),d1+l+d2)|((t1, u1), d1)<-ps, -- (t1,..u1=a,  b=t2..u2) => (t1,u2)
                                       ((t2, u2), d2)<-ps, u1==a && t2==b]
        ps = M.toList table
------------------------------------------------------
handler (clusters, table) path = (newClusters, newTable)
  where newPaths    = findNewPaths table path                      -- 1. find new paths
        newTable    = M.union table (M.fromList newPaths)          -- 2. new table = old table + new path
        checkNodes  = nub.join $ [[a,b]| ((a,b),_)<-newPaths]      -- 3. toBeChecked nodes = nodes in new paths
        clusters'   = updateClusters clusters checkNodes newTable  -- 4. update old clusters with toBeChecked nodes
        newClusters = clusters' ++ [(d,[a,b])|((a,b),d)<-newPaths] -- 5. add trivial clusters
------------------------------------------------------
egalitarianism3Easy n xs ys lengths
    | n <= 2    = n
    | otherwise = maximum                     $ --- 6. max size   = maximum [nodesize]
                  map (length.snd)            $ --- 5. node sizes = [nodesize] = [length [nodes]]
                  fst                         $ --- 4. clusters   = [cluster]  = [(distance,[nodes])]
                  foldl handler ([], M.empty) $ --- 3. last state = (clusters, pathTable)
                  map (\(x,y,l)->((x,y),l))   $ --- 2. paths      = [((a,b),l)]
                  zip3 xs ys lengths            --- 1. pathInfos  = [( a,b, l)]
------------------------------------------------------
main = do
    print $ egalitarianism3Easy 4  [1,1,1]
                                   [2,3,4]
                                   [1,1,1]

    print $ egalitarianism3Easy 6  [1,2,4,2,3]
                                   [2,5,3,3,6]
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