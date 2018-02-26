-- Split the graph into it's connected components

type Node = Int
type Edge = (Node, Node)
type Graph = [Node] [Edge]

depthFirst :: Graph -> Node -> [Node]
depthFirst (v, e) n
    | [x | x <- v, x == n] == [] = []
    | otherwise = dfrecursive (v, e) [n]

dfrecursive :: Graph -> [Node] -> [Node]
dfrecursive ([], _) _ = []
dfrecursive (_, _) [] = []
dfrecursive (v, e) (top:stack)
    | [x | x <- v, x == top] == [] = dfrecursive (newv, e) stack
    | otherwise = top : dfrecursive (newv, e) (adjacent ++ stack)
    where
        newv = [x | x <- v, x /= top]
        adjacent = [x | (x,y) <- e, y == top] ++ [x | (y,x) <- e, y == top]

connectedcomponents :: Graph -> [[Node]]
connectedcomponents ([],_) = []
connectedcomponents (top:v,e) 
    | remaining == [] = [connected]
    | otherwise = connected : connectedcomponents (remaining, e)
    where
        connected = depthfirst (top:v,e) top
        remaining = (top:v) \\ connected
        