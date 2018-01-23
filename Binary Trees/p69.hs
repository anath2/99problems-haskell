-- Docstring representation of binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a)

-- Use preorder traversal
treeToDocstring :: Tree Char -> String
treeToDocstring Empty = "."
treeToDocstring (Branch "x" l r) = "x": (treeToDocstring l ++ treeToDocstring r)

-- Use docstring to Tree
docstringToTree :: String -> Tree Char
docstringToTree [] = (Empty, "")
docstringToTree ('.':xs) = (Empty, xs)

docstringToTree (x:xs)  = (Branch x left right, rest2)                                                                                                                 
  where (left,rest) = ds2tree xs                                                                                                                               
        (right,rest2) = ds2tree rest 