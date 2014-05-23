import Data.Maybe
import Data.List
import Data.Maybe
import Data.List

inf = 1/0
--
data Graph vertexValue edgeValue = Graph [(Int,vertexValue)] [(Int,Int,edgeValue)]
--
weight Nothing = 0
weight (Just (_, _, x)) = x
--
findMinVertex (v:vs) = findMin (v:vs) inf v where
                                            findMin [] min vertex = vertex
                                            findMin (v:vs) min candidate | (snd v < min) = findMin vs (snd v) v
                                                                                    | otherwise = findMin vs min candidate
--
isNeighbours v1 v2 edges | (getEdge v1 v2 edges == Nothing) = False
                                    | otherwise = True
--
getEdge  v1 v2 edges = getRightEdge (filter ( \(a,b,_) -> fst v1 == a && fst v2 == b || fst v1 == b && fst v2 == a) edges) where
                              getRightEdge [] = Nothing
                              getRightEdge (x:xs) = Just x

--Scanning numbers of vertices
pop [] v = []
pop (l:ls) v    | (fst v == fst l) = ls
                   | otherwise = l:(pop ls v)

--Trying to change distances
relaxed distancesList minVertex edges = map (\v -> if (isNeighbours minVertex v edges) then 
                                                                              ( fst v, min (snd v) (snd minVertex + weight (getEdge minVertex v edges)) ) 
                                                                         else v) distancesList
--
relaxMinAndPop distancesList minVertex edges = pop (relaxed distancesList minVertex edges) minVertex


findShortest (Graph vertices edges) startVertexNumber = findShort (map (\(x,_) -> if (x == startVertexNumber) then (x,0) else (x,inf)) vertices) [] where 
              findShort [] visited = visited
              findShort notVisited visited = findShort (relaxMinAndPop (notVisited) (findMinVertex notVisited) edges) ((findMinVertex notVisited):visited)

main = do
    -- from wikipedia
    let graph = Graph [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0)] [(1,3,9),(1,2,7),(1,6,14),(2,3,10),(2,4,15),(6,3,2),(6,5,9),(3,4,11),(5,4,6)]
    putStrLn(show $ findShortest graph 1)