-- Srm631Div2Lev2 catsOnTheLine
-- http://community.topcoder.com/stat?c=problem_statement&pm=13392
module Srm631Div2Lev2 where
import Data.Maybe
import Data.List
import Control.Monad
------------------------------------------------------
cJust cond value = if cond then (Just value) else Nothing
------------------------------------------------------
handler ends (lo,hi,cnt) = cJust isPossible (ends++[end])
    where (start,end) = (max (1 + last ends) lo, start + cnt - 1)
          isPossible  = end <= hi
------------------------------------------------------
handler' events evt  = cJust isPossible events'
    where events'    = insert evt events 
          isPossible = isJust $ foldM_ handler [-2001] events'
------------------------------------------------------
catsOnTheLine ps cs time = toString $ foldM_ handler' [] events
    where events     =  [(p-time, p+time, cnt)|(p,cnt)<-zip ps cs]
          toString v = if isJust v then "Possible" else "Impossible"
------------------------------------------------------
main = do
        print $ catsOnTheLine [0] [7] 3
        print $ catsOnTheLine [0] [8] 2
        print $ catsOnTheLine [0,1] [3,1] 0
        print $ catsOnTheLine [5, 0, 2] [2, 3, 5] 2
        print $ catsOnTheLine [5, 1, -10, 7, 12, 2, 10, 20] [3, 4, 2, 7, 1, 4, 3, 4] 6

{-| Output
"Possible"
"Impossible"
"Impossible"
"Impossible"
"Possible"
-}

-------------------------------------------------------
-- need optimiazation setting ? see below
------------------------------------- -----------------
handlerOpt (ends,events) evt  = cJust isPossible (ends',events')
    where prevEvents  = [e|e<-events, e < evt]
          prevEnds    = take (length prevEvents) ends
          nextEvents  = [evt] ++ [e|e<-events, evt < e]
          nextEnds    = foldM handler [last prevEnds] nextEvents
          events'     = prevEvents ++ nextEvents
          ends'       = prevEnds   ++ tail (fromJust nextEnds)
          isPossible  = isJust nextEnds
------------------------------------------------------
catsOnTheLineOpt ps cs time = toString $ foldM_ handlerOpt init events
    where events     =  [(p-time, p+time, cnt)|(p,cnt)<-zip ps cs]
          toString v = if isJust v then "Possible" else "Impossible"
          init = ([-2001], [(-2001,-2001,0)])
------------------------------------------------------

