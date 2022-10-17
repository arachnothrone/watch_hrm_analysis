{-
String operations
-}

module StringOp where


splitAtChar :: Char -> String -> (String, Bool, String)
splitAtChar _ [] = ([], False, [])
splitAtChar c inStr 
    | c == head inStr   = ([], True, drop 1 inStr)
    | exist             = (head(inStr):lhs, True, rhs)
    | otherwise         = ([], False, inStr)
    where
        (lhs, exist, rhs) = splitAtChar c (tail inStr)

unquote :: Char -> String -> String
unquote c inStr
    | exist1 && exist2 = lhs2
    | otherwise = ""
    where
        (_, exist1, rhs1) = splitAtChar c inStr
        (lhs2, exist2, _) = splitAtChar c rhs1

isSubString :: String -> String -> Bool
isSubString (_:_) [] = False
isSubString xs ys 
    | isPrefix xs ys = True
    | isSubString xs (tail ys) = True
    | otherwise = False

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix (_:_) [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

splitAtSubstring :: String -> String -> (String, Bool, String)
splitAtSubstring [] [] = ([], False, [])
splitAtSubstring xs [] = (xs, False, [])
splitAtSubstring xs ys
    | isPrefix xs ys = ([], True, drop (length xs) ys)
    | b = (head(ys):a, True, c)
    | otherwise = ([], False, ys)
    where
        (a, b, c) = splitAtSubstring xs (tail ys)

