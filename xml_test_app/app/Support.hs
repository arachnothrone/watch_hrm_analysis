-- support file
--
--
import System.IO
import Data.Char(toUpper)
import Data.Maybe

main :: IO()
main = do
    -- inHndlr <- openFile "src_data/apple_health_export/export.xml" ReadMode  
    inHndlr <- openFile "src_data/healthdata_example.xml" ReadMode
    outHndlr <- openFile "output_new.txt" AppendMode -- WriteMode
    inputStr <- hGetContents inHndlr
    -- putStrLn inputStr
    hPutStr $ modifier inputStr
    -- hPutStr outHndlr (map toUpper inputStr)
    --hPutStr outHndlr (modifier inputStr)
    hClose inHndlr
    hClose outHndlr
    putStrLn "--> end"

modifier :: String -> String
modifier s
    | isPrefix "<Record" s = case parameter_type of 
        "HKQuantityTypeIdentifierHeartRate"    -> parameter_startDate ++ ',':parameter_endDate ++ ',':parameter_value
        _                                      -> ".. "
    | otherwise = ""
    where
        parameter_type = unwrapMaybe $ getParamValue "type" s 
        parameter_startDate = unwrapMaybe $ getParamValue "startDate" s 
        parameter_endDate = unwrapMaybe $ getParamValue "endDate" s 
        parameter_value = unwrapMaybe $ getParamValue "value" s 

unwrapMaybe :: Maybe String -> String
unwrapMaybe (Just s) = s 
unwrapMaybe Nothing = "" 

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

getParamValue :: String -> String -> Maybe String
getParamValue parameter line
    | parExists == True = Just $ unquote rhs
    | otherwise = Nothing
    where
        (_, parExists, rhs) = splitAtSubstring parameter line

unquote :: String -> String
unquote inStr
    | exist1 && exist2 = lhs2
    | otherwise = ""
    where
        (_, exist1, rhs1) = splitAtChar '\"' inStr
        (lhs2, exist2, _) = splitAtChar '\"' rhs1

splitAtChar :: Char -> String -> (String, Bool, String)
splitAtChar _ [] = ([], False, [])
splitAtChar c inStr 
    | c == head inStr   = ([], True, drop 1 inStr)
    | exist             = (head(inStr):lhs, True, rhs)
    | otherwise         = ([], False, inStr)
    where
        (lhs, exist, rhs) = splitAtChar c (tail inStr)

