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

splitAtSubstring :: String -> String -> (String, Bool, String)
splitAtSubstring [] [] = ([], False, [])
splitAtSubstring xs [] = (xs, False, [])
splitAtSubstring xs ys
    | isPrefix xs ys = ([], True, drop (length xs) ys)
    | b = (head(ys):a, True, c)
    | otherwise = ([], False, ys)
    where
        (a, b, c) = splitAtSubstring xs (tail ys)