-- Construct a height balanced binary tree from a given number of nodes

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
  where
        -- baltree h n = weight-balanced trees of height h with n nodes
        -- assuming minNodes h <= n <= maxNodes h
        baltree 0 n = [Empty]
        baltree 1 n = [Branch x Empty Empty]
        baltree h n = [Branch x l r |
                (hl,hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
                let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                nl <- [min_nl .. max_nl],
                let nr = n - 1 - nl,
                l <- baltree hl nl,
                r <- baltree hr nr]
