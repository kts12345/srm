-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List  -- for nub
import Data.Maybe -- for Just
import qualified Data.Map.Strict as M  -- fromList, toList, union
------------------------------------------------------
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
findAllNewPaths :: M.Map (Int,Int) Int -> ((Int,Int), Int) -> [((Int,Int), Int)] 
findAllNewPaths pathTable ((a,b),l) = [((a,b),l)] ++ add (a,b) ++ add (b,a)
    where add (a, b) = [((a,u),l+d)|((t,u),d)<-ps, t==b]      -- (a,    b=t,..u       ) => (a,u  )
                    ++ [((t,b),d+l)|((t,u),d)<-ps, u==a]      -- (        t,..u=a,  b ) => (t,b  )
                    ++ [((t1,u2),d1+l+d2)|((t1, u1), d1)<-ps, -- (t1,..u1=a,  b=t2..u2) => (t1,u2)
                                          ((t2, u2), d2)<-ps, u1==a && t2==b]
          ps = M.toList pathTable
------------------------------------------------------
handler :: ([(Int, [Int])], M.Map (Int, Int) Int) -> (Int, Int, Int)-> ([(Int, [Int])], M.Map (Int,Int) Int)
handler (clusters, pathTable) (a,b,l) = (clusters'', pathTable')
    where newPaths   = findAllNewPaths pathTable ((a,b),l)
          pathTable' = M.union pathTable $ M.fromList newPaths
          nodes      = nub $ foldl (\s ((a,b),l)->s++[a,b]) [] newPaths
          clusters'  = updateClusters clusters nodes pathTable'
          clusters'' = clusters' ++ map makeCluster newPaths
------------------------------------------------------
egalitarianism3Easy n xs ys lengths
    | n <= 2    = n
    | otherwise = maximum                     $ --- max size   = maximum [nodesize]
                  map (length.snd)            $ --- node sizes = [nodesize] = [length [nodes]]
                  fst                         $ --- clusters   = [cluster]  = [(distance,[nodes])]
                  foldl handler ([], M.empty) $ --- last state = (clusters, pathTable)
                  zip3 xs ys lengths            --- pathInfos  = [(a,b,l)]
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