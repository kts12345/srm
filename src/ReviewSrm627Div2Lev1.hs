module ReviewSrm627Div2Lev1 where
import qualified Data.Map as M

countTable tbl key = M.insertWith (+) key 1 tbl

howManySquares xs = last
    $ map    sum
    $ map    (map truncate)
    $ map    (map (/4))
    $ map    M.elems
    $ scanl  countTable M.empty 
    $ xs

main = do
    print $ howManySquares [1,1,2,2,1,1,2]
    print $ howManySquares [3,1,4,4,4,10,10,10,10]
    print $ howManySquares [1,2,3,4,1,2,3,4,1,2,3,1,2,3,4,1,2,3,3,3]
    print $ howManySquares [1,1,1,2,2,2,3,3,3,4,4,4]
    print $ howManySquares [1,1,1,2,1,1,1,3,1,1,1]
    print $ howManySquares [2,2,4,4,8,8]

-- last $ map f1 $ map f2 $ map f3 scanl .. == f1 $ f2 $ f3 $ fold ..