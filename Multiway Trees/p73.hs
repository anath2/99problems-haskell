-- Lisp like representation of trees
data Tree a = Node a [Tree a]
newtype P a = P { runP :: String -> Maybe (a, String) }
data Syntax a = Syntax {
                    display :: a -> String,
                    parse :: P a
                }   

lisp :: Syntax (Tree Char)
lisp = iso toTree fromTree
        (literal '(' *> (char <*> list (space *> lisp) (literal ')')) <|> char)
  where toTree (Left (x, ts)) = Node x ts
        toTree (Right x) = Node x []
        fromTree (Node x []) = Right x
        fromTree (Node x ts) = Left (x, ts)