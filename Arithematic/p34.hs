-- Calculate the Euler totient for a number
totientPhi :: Int -> Int
totientPhi n = length [x | x <- [1..(n-1)], gcd x n == 1]

