import Data.Maybe
import Data.List
import Data.Ord

------------------------------------------------------
handler time upper (position, count) = newUpper'
    where
        newLower  = max (upper + 1) (position - time)
        newUpper  = newLower + count -1
        newUpper' = if position + time < newUpper then Nothing else Just newUpper

------------------------------------------------------         
sortedCats time = foldl handler' (Just (-2001)) 
    where        
       handler'  Nothing _ = Nothing
       handler'  u   (p,c) = handler time (fromJust u) (p,c)
       
------------------------------------------------------
catsOnTheLine ps cs time = if result == Nothing then "Impossible" else "Possible"
    where result = sortedCats time $ sortBy (comparing fst) $ zip ps cs

------------------------------------------------------
catsOnTheLine' ps cs time  = 4
    where 
        handler accs (p,c) = 5
            where
                before = [(p',c')| (p',c',u')<-accs, p' < p]
                after  = [(p',c')| (p',c',u')<-accs, p' < p]
        handler' Nothing _ = Nothing
        handler' acc (p,c) = handler (fromJust acc) (p,c)


-- | The main entry point.
main :: IO ()
main = do
        print $ catsOnTheLine [0] [7] 3
        print $ catsOnTheLine [0] [8] 2
        print $ catsOnTheLine [0,1] [3,1] 0
        print $ catsOnTheLine [5, 0, 2] [2, 3, 5] 2
        print $ catsOnTheLine [5, 1, -10, 7, 12, 2, 10, 20]
                              [3, 4,   2, 7,  1, 4,  3,  4]
                              6


{-| Output
[(0,7)]
[(0,8)]
[(0,3),(1,1)]
[(0,3),(2,5),(5,2)]
[(-10,2),(1,4),(2,4),(5,3),(7,7),(10,3),(12,1),(20,4)]
-}