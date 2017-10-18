-- Slice a list, index starting from 1 
-- Both starting and ending index being included

sliceList :: [a] -> Int -> Int -> [a]
sliceList xs a b = drop (a - 1) $ take b xs

-- TODO : Do negative indices