-- Ditermine if an integer is prime or not
isPrime :: Int -> Boolean
isPrime k
    | k < 2 = False
    | otherwise = null [x | x <- [2..(k - 1)], k `mod` x == 0]
    