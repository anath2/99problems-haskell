-- Calculate euler totient using prime factors and their multiplicites

primeFactors' 1 = []
primeFactors' n = encode $ helper n 2 
    where
        helper 1 _ = []
        helper n f 
            | n `mod` f == 0 = f : helper (n `div` f) f
            | otherwise      = helper n (f + 1)
        encode = foldr acc []
            where
                acc x [] = [(x, 1)]
                acc x (y@(num, count):ys) 
                    | x == num = (num, count + 1):ys
                    | otherwise = (x, 1):y:ys

eulerTotient' :: Int -> Int
                    
eulerTotient' m = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactors' m]
