-- Solve an expression expressed in reverse Polish notation
-- Reverse polish notation is a form of postfix notation For example, 
-- the expression 10 - (4 + 3) * 2 can be expressed as 10 4 3 + 2 * -

import Data.List

reversePolishNotation :: (Num a) => Str -> a
reversePolishNotation expression = head (foldl foldingFunction [] (words expression))
--[WIP]