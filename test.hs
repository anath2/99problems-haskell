-- Haskell playground

bmi :: (RealFloat a) => a -> a -> String
bmi weight height
    | result <= 18.5 = "You are underweight"
    | result <= 25.0 = "You are perfectly normal"
    | result <= 30.0 = "You are overweight"
    | otherwise = "You are too fat"
    where result = height / weight ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + (topArea * 2)
