-- Get prime factors with their multiplicity
primeFactors' :: Int -> [(Int, Int)]

primeFactors' n = runLengthEncode . helper
    where ...