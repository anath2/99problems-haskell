-- Construct a tree from nodestring
data Tree a = Node a [Tree a]

stringToTree :: String -> Tree Char
stringToTree (x:xs@(y:ys)) 
     | y == '^'  = Node x []
     | otherwise = Node x (map stringToTree subs)
               where subs = snd $ foldl parse ([],[]) $ init xs
	             parse ([],[])      z = ([z], [[z]])
	             parse (stack, acc) z = (stack', acc')
			       where stack' 
			               | z == '^'  = init stack
				       | otherwise = stack ++ [z]
			             acc'   = if stack == []
				                then acc ++ [[z]]