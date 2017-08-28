-- Check if the list is a palidrome

isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome xs = xs == reverse xs