-- Goldbach's conjecture

goldbachConjecture :: Int -> (Int, Int)
goldbachConjecture n
    | n `mod` 2 /= 0 = error "Number not even"
    |otherwise = head [(x, n - x) | x <- [2..(n-1)], isPrime x, isPrime (n - x)]
    where
        isPrime n = null [x | x <- [2..(n-1)], n `mod` x == 0]
    