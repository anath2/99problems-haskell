-- Depth first traversval of a graph

type Node = Int
type Edge = (Node, Node)
type Graph = ([Node], [Edge])

depthFirst :: Graph -> Node -> [Node]
depthFirst (v, e) n 
    | [ x | x <- v, x == n ] == [] = []
    | otherwise = dfRec (v, e) [n]

dfRec :: Graph  -> [Node] -> [Node]
dfRec ([], _) _     = []
dfRec (_, _) []     = []
dfRec (v,e) (top:stack)
    | [ x | x <- v, x == top ] == [] = dfRec (newv, e) stack
    | otherwise = top : dfRec (newv, e) (adjacent ++ stack)
    where
        adjacent = [ x | (x,y) <- e, y == top] ++ [ x | (y,x) <- e, y == top ]
        newv = [ x | x<-v, x /= top ]
        