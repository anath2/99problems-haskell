-- GCD Using Euclid's algorithm

gcdEuclid ::  Int -> Int -> Int
gcdEuclid a b
    |a > b = error "First number should be less than second"
    | b `mod` a  == 0 = (b `div` a)    
    | b `mod` a == 1 = 1
    | otherwise = gcdEuclid (b `mod` a) a 