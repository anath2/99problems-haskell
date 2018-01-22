-- Given a tree find its preorder and inorder traversal
data Tree a = Empty | Branch a (Tree a) (Tree a) 
    deriving (Eq, Show)

preorderTraversal :: Tree Char -> String
preorderTraversal Empty = ""
preorderTraversal (Branch a l r) = a : preorderTraversal l ++ preorderTraversal r 

inorderTraversal :: Tree Char -> String
inorderTraversal Empty = ""
inorderTraversal (Branch a l r) = inorder l ++ a : (inorder r)

-- WIP Tree from preorderTraversal and inorderTraversal strings