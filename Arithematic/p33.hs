-- Check if the numbers are co-prime

checkCoPrime :: Int -> Int -> Bool
checkCoPrime a b = (gcd a b) == 1
        