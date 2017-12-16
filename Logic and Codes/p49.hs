-- Gray code

grayCodes :: Int -> [String]
grayCodes 0 = [""]
grayCodes n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ grayCodes (n - 1)