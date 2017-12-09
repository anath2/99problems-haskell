-- Define built in boolean operations
not' = not 
and'= (&&)
or' = (||)
eq' = (==)

-- Custom boolean operations
nxor' a b  = foldr eq' True [a, b]
xor' a b = not $ nxor' a b

nand' a b = not' $ and' a b
nor' a b = not' $ or' a b
impl' a b = or' (not' a) b

table :: (Bool -> Bool -> Bool) -> IO()
table f = putStrLn $ concatMap (++ "\n") 
            [show a ++ " " ++ show b ++ " " ++ show (f a b) 
                | a <- [True, False], b <- [True, False]]