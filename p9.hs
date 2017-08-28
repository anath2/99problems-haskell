-- ADD THE REPEATING ELEMENTS OF A LIST INTO A SUBLIST
{--
    CHECK IF THE HEAD OF TAIL IS SAME AS HEAD
    IF YES
        ADD THE FIRST AND SECOND ELEMENT TO A SUBLIST
    NOW THE HEAD BECOMES TWO ELEMENTS 
--}

-- grp [] = []
-- grp (x:xs) = (x: filter (==x) xs) : (grp $ filter (/=x) xs) 
-- THE FOLLOWING SOLUTION IS WRONG AS IT TAKES PACKS EVEN NON CONSECUTIVE ELEMENTS

-- packList [] = []
-- packList (x:xs) = let (first, last) = span (==x) xs
--     in (x:first): packList last
-- This doesn't work either because it packs everything