-- Arithematic puzzle (Insert operators in a list of integers)

import Control.Monad
import Data.List
import Data.Maybe
 
type Equation = (Expr, Expr)
data Expr = Const Integer | Binary Expr Op Expr
        deriving (Eq, Show)
data Op = Plus | Minus | Multiply | Divide
        deriving (Bounded, Eq, Enum, Show)
type Value = Rational