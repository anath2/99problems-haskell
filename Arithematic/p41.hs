-- Get all even numbers in a range with their goldbach conjectures

goldbachConjecture :: Int -> (Int, Int)
goldbachConjecture n
    | n `mod` 2 /= 0 = error "Number not even"
    |otherwise = head [(x, n - x) | x <- [2..(n-1)], isPrime x, isPrime (n - x)]
    where
        isPrime n = null [x | x <- [2..(n-1)], n `mod` x == 0]

goldbachRange :: Int -> Int -> [(Int, Int, Int)]
goldbachRange a b 
    | a `mod` 2 == 0 = foldr goldbach [] [a, (a + 2)..b] 
    | otherwise = foldr goldbach [] [a + 1, (a + 3)..b]
        where goldbach n xs = (n : (goldbachConjecture n)) : xs