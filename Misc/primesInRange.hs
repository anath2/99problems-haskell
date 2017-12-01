-- Find primes in a range
primesInRange :: Int -> [Int]
primesInRange n 
    | n < 3 = []
    | otherwise = [2] ++ [x | x <- [3..(n - 1)], isPrime x ]
        where
            isPrime n = null [x | x <- [2..(n-1)], n `mod` x == 0]