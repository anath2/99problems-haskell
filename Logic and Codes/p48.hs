-- Generalize solution from problem p46 to any number of inputs
import Control.Monad (replicateM)

-- Define built in boolean operations
not' = not 
and'= (&&)
or' = (||)

-- Custom boolean operations
equ'  a b = a == b
nxor' a b  = foldr equ' True [a, b]
xor' a b = not $ nxor' a b

nand' a b = not' $ and' a b
nor' a b = not' $ or' a b
impl' a b = or' (not' a) b

-- Create infix operators
-- functions as in solution 46
-- functions as in solution 46
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` -- was 7, changing it to 3 got me the same results as in the original question :(
 
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  | "
          space False = " | "
