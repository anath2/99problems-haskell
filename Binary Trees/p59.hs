-- Construct a list of all height balanced binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbaltree :: a -> Int -> [Tree a]
hbaltree x 0 = [Empty]
hbaltree x 1 = [Branch x Empty Empty]
hbaltree x h = [Branch x l r |
        (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
        l <- hbaltree x hl, r <- hbaltree x hr]