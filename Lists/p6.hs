-- Reverse a list
reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = (reverse' (xs) ++ [x])

-- Check if the list is a palidrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse' xs