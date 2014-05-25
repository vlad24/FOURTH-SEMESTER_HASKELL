import Data.Maybe
import Data.List

inf = 1/0
--
data Graph vertexValue edgeValue = Graph [(Int,vertexValue)] [(Int,Int,edgeValue)]

--Get the weight of the edge
weight Nothing = 0
weight (Just (_, _, x)) = x

--Find the vertex with min current distance to it
findMinVertex (v:vs) = findMin (v:vs) inf v where
                                            findMin [] min vertex = vertex
                                            findMin (v:vs) min candidate | (snd v < min) = findMin vs (snd v) v
                                                                                    | otherwise = findMin vs min candidate

--Find out whether two given verteces are neighbours
isNeighbours v1 v2 edges | (getEdge v1 v2 edges == Nothing) = False
                                    | otherwise = True

--Try to find an edge connecting two given vertices
getEdge  v1 v2 edges = getRightEdge (filter ( \(a,b,_) -> (fst v1 == a) && (fst v2 == b) || (fst v1 == b) && (fst v2 == a)) edges) where
                              getRightEdge [] = Nothing
                              getRightEdge (x:xs) = Just x

--Pop the given vertex
pop [] v = []
pop (l:ls) v    | (fst v == fst l) = ls
                   | otherwise = l:(pop ls v)

--Trying to change distances of the incident vertices
relaxed distancesList minVertex edges = map (\v -> if (isNeighbours minVertex v edges) then 
                                                                              ( fst v, min (snd v) (snd minVertex + weight (getEdge minVertex v edges)) ) 
                                                                         else v) distancesList

--Relax the min all the incident vertices of the minVertex and pop it
relaxMinAndPop distancesList minVertex edges = pop (relaxed distancesList minVertex edges) minVertex

--
findShortest (Graph vertices edges) startVertexNumber = findShort (map (\(x,_) -> if (x == startVertexNumber) then (x,0) else (x,inf)) vertices) [] where 
              findShort [] visited = visited
              findShort notVisited visited = findShort (relaxMinAndPop (notVisited) (findMinVertex notVisited) edges) ((findMinVertex notVisited):visited)
--
dijkstra (Graph vertices edges) startVertexNumber = map (\(v,d) -> if (d == inf) then (v, Nothing) else (v,Just d)) (findShortest (Graph vertices edges) startVertexNumber)

main = do
    -- from wikipedia
    let graph = Graph [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0)] [(1,3,9),(1,2,7),(1,6,14),(2,3,10),(2,4,15),(6,3,2),(3,4,11)]
    putStrLn(show $ dijkstra graph 1)