-- Path from one node to another

import List (elem)
 
paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths a b g = paths1 a b g []
 
paths1 :: Eq a => a -> a -> [(a,a)] -> [a] -> [[a]]
paths1 a b g current = paths2 a b g current [ y | (x,y) <- g, x == a ]
 
paths2 :: Eq a => a -> a -> [(a,a)] -> [a] -> [a] -> [[a]]
paths2 a b g current []	| a == b = [current++[b]]
			| otherwise = []
paths2 a b g current (x:xs) | a == b = [current++[b]] 
			    | elem a current = []
			    | otherwise = (paths1 x b g (current++[a])) ++ (paths2 a b g current xs)
