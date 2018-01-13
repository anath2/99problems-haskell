-- Construct a complete binary tree

-- DEFINITION :
-- A complete binary tree is a binary tree if all levels except probably
-- the last one has two children each

-- NOTE : 
-- Consider assigning an address to each node of the tree
-- Starting at root equals 1
-- For each node X, the following property holds
-- The address of X's left would be 2*A, right's would be 2*A + 1
-- if they exist

import Data.List
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

filled :: Tree a -> [[Bool]]
filled Empty = repeat [False]
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
    where
        generate_tree x 
            | x > n     = Empty
            | otherwise = Branch 'x' (generate_tree x*2) (generate_tree (x*2 + 1))

isCompleteBinaryTree :: Tree a -> Bool                                                                                                                      
isCompleteBinaryTree Empty = True                                                                                                                           
isCompleteBinaryTree t = and $ last_proper : zipWith (==) lengths powers                                                                                    
    where levels      = takeWhile or $ filled t                                                                                                                  
        -- The upper levels of the tree should be filled.                                                                                                      
        -- Every level has twice the number of nodes as the one above it,                                                                                      
        -- so [1,2,4,8,16,...]                                                                                                                                 
        lengths     = map (length . filter id) $ init levels                                                                                                   
        powers      = iterate (2*) 1                                                                                                                           
        -- The last level should contain a number of filled spots,                                                                                             
        -- and (maybe) some empty spots, but no filled spots after that!                                                                                       
        last_filled = map head $ group $ last levels                                                                                                           
        last_proper = head last_filled && (length last_filled) < 3
            