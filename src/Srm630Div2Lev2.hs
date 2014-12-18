-- Srm630Div2Lev2 Egalitarianism3Easy
-- http://community.topcoder.com/stat?c=problem_statement&pm=13376
module Srm630Div2Lev2 where
import Data.List
import Control.Arrow
import qualified Data.Map as M

------------------------------------------------------
frequency xs = map (head &&& length) $ group $ sort xs 
------------------------------------------------------
addEdge (fs,ps) (a,b,l) = (fs', ps++ps')
    where ps' = [(a,b,l), (b,a,l)]              -- (a,b),(b,a)
             ++ [(a,u,l+d)|(t,u,d)<-ps, t==b]   -- (a,b)++(b,..u)        => (a,u)  
             ++ [(t,b,d+l)|(t,u,d)<-ps, u==a]   --        (t,..a)++(a,b) => (t,b)
             ++ [(b,u,d+l)|(t,u,d)<-ps, t==a]   -- (b,a)++(a,..u)        => (b,u)
             ++ [(t,a,d+l)|(t,u,d)<-ps, u==b]   --        (t,..b)++(b,a) => (t,a)
             ++ [(t1,u2,d1+l+d2) | (t1,u1,d1)<-ps, (t2,u2,d2)<-ps, u1==a, t2==b]
             ++ [(t1,u2,d1+l+d2) | (t1,u1,d1)<-ps, (t2,u2,d2)<-ps, u1==b, t2==a]
          fs' = M.toList $ M.fromListWith (+) $ fs ++ (frequency $ map (\(_,_,d)->d) ps')

------------------------------------------------------
egalitarianism3Easy n xs ys lengths = 
  --  if n == 1 then 1 else length $ 
  fst $ foldl addEdge ([(0,0)],[]) $ zip3 xs ys lengths

------------------------------------------------------
main = do
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


{- Output
3
3
9
2
1
-}