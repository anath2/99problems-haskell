-- Display the tree on a grid based on the following rule
-- Along x axis -> Position in inorder traversal of the tree
-- Along y axis -> Depth of the node

data Tree a = Empty | Branch a (Tree a) (Tree a)

type Pos = (Int, Int)

-- Assuming (1, 1) be the top left corner of the layout

layout :: Tree a -> Tree (a, Pos)
layout t = fst (layoutAux 1 1 t)
  where layoutAux x y Empty = (Empty, x)
        layoutAux x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
          where (l', x')  = layoutAux x (y+1) l
                (r', x'') = layoutAux (x'+1) (y+1) r