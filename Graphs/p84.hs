-- Create a minimum spanning tree from a graph

import Data.List
import Array

type Graph n w = Array n [(n,w)]

mkGraph dir bnds es =
    accumArray (\xs x -> x:xs) [] bnds
               ([(x1,(x2,w)) | (x1,x2,w) <- es] ++
               if dir then []
               else [(x2,(x1,w)) | (x1,x2,w) <- es, x1 /= x2])

adjacent g v = map fst (g!v)

nodes g = indices g

edgeIn g (x,y) = elem y (adjacent g x)

weight x y g = head [c | (a,c) <- g!x, a == y]

edgesD g = [(v1,v2,w) | v1 <- nodes g, (v2,w) <- g!v1]
edgesU g = [(v1,v2,w) | v1 <- nodes g, (v2,w) <- g!v1, v1 < v2]

prim g = prim' [n] ns []
    where (n:ns) = nodes g
          es = edgesU g
          prim' t [] mst = mst
          prim' t r mst = let e@(c,u',v') = minimum
                                             [(c,u,v) | (u,v,c) <- es,
                                                         elem u t,
                                                         elem v r]
                          in  prim' (v':t) (delete v' r) (e:mst)