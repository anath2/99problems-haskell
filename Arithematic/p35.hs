-- Get a list prime factors for a number

-- Check if a number is prime
isPrime :: Int -> Boolean
isPrime k
    | k < 2 = False
    | otherwise = null [x | x <- [2..(k - 1)], k `mod` x == 0]


listPrimeFactors :: Int -> [Int]
listPrimeFactors 1 -> []