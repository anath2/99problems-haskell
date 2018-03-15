-- Transform the syntax diagram given in problem 
-- 96 to a system of syntax diagrams that do not
-- contain work

import Data.Char
syntaxCheck :: String -> Bool
syntaxCheck []      = False

syntax_check (x:xs) = isLetter x && loop xs
    where loop [] = True
          loop (y:ys) | y == '-'     = (not . null) ys && isAlphaNum (head ys) && loop (tail ys) 
                      | isAlphaNum y = loop ys
                      | otherwise    = False