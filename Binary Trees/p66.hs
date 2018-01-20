-- Generate a string representation of a binary treee
data Tree a = Empty | Branch a (Tree a) (Tree a)

treeToString :: Tree Char -> String
-- Case : Empty tree
treeToString Empty = ""
-- Case : Tree with one node
treeToString (Branch x (Empty) (Empty)) = [x]
treeToString (Branch x l r) = x : '(' : ((treeToString l) ++ "," ++ (treeToString r) ++ ")")