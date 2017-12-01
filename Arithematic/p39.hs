-- Find primes in a range
primesInRange :: Int -> Int -> [Int]
primesInRange a b 
    | b < 3 = []
    | a < 3 = [x | x <- [3..(b - 1)], isPrime x]
    | otherwise = [x | x <- [a..(b - 1)], isPrime x ]
        where
            isPrime n = null [x | x <- [2..(n-1)], n `mod` x == 0]
