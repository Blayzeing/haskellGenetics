
getTillCard :: [Char] -> Char -> [Char]
getTillCard [] c = []
getTillCard (h:t) c
    | h == c = [c]
    | otherwise = h:(getTillCard t c)
