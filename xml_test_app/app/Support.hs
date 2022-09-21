-- support file
--
--
import System.IO
import Data.Char(toUpper)

main :: IO()
main = do
    -- inHndlr <- openFile "src_data/apple_health_export/export.xml" ReadMode  
    inHndlr <- openFile "src_data/healthdata_example.xml" ReadMode
    outHndlr <- openFile "output.txt" AppendMode -- WriteMode
    inputStr <- hGetContents inHndlr
    hPutStr outHndlr (map toUpper inputStr)
    hClose inHndlr
    hClose outHndlr

modifier :: String -> String
modifier s
    | isPrefix "<Record" s = undefined

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

splitAtSub :: String -> String -> (String, String)
splitAtSub [] ys = ([], ys)
splitAtSub xs [] = ([], [])
splitAtSub xs ys
    | isPrefix xs ys = ([], drop (length xs) ys)
    | otherwise = ([], [])

isSubStringMore :: String -> String -> (String, Bool, String)
isSubStringMore [] [] = ([], False, [])
isSubStringMore xs [] = (xs, False, [])
isSubStringMore xs ys
    | isPrefix xs ys = ([], True, drop (length xs) ys)
    | b = (head(ys):a, True, c)
    | otherwise = ([], False, [])
    where
        (a, b, c) = isSubStringMore xs (tail ys)