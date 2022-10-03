-- support file
--
--
import System.IO
import Data.Char(toUpper)
import Data.Maybe

main :: IO()
main = do
    --inHndlr <- openFile "src_data/apple_health_export/export.xml" ReadMode  
    inHndlr <- openFile "src_data/healthdata_example.xml" ReadMode
    outHndlr <- openFile "output_new.txt" AppendMode -- WriteMode
    hSetBuffering inHndlr LineBuffering
    
    -- no explicit loop
    -- inputStr <- hGetContents inHndlr
    -- hPutStr stdout (unlines . map modifier . lines $ inputStr)
    
    -- with explicit loop
    mainLoop inHndlr stdout

    hClose inHndlr
    hClose outHndlr
    putStrLn "--> end"

mainLoop :: Handle -> Handle -> IO()
mainLoop inh outh = do
    ineof <- hIsEOF inh
    if ineof
        then return()
        else do
            inpStr <- hGetLine inh 
            hPutStr outh (modifier inpStr)
            mainLoop inh outh

modifier :: String -> String
modifier s
    | exist = case parameter_type of 
        "HKQuantityTypeIdentifierHeartRate"    -> parameter_startDate ++ ',':parameter_endDate ++ ',':parameter_value ++ "\n"
        _                                      -> ""
    | otherwise = ""
    where
        (_, exist, rhs) = splitAtSubstring "Record" s 
        parameter_type = unwrapMaybe $ getParamValue "type" rhs
        parameter_startDate = unwrapMaybe $ getParamValue "startDate" rhs 
        parameter_endDate = unwrapMaybe $ getParamValue "endDate" rhs 
        parameter_value = unwrapMaybe $ getParamValue "value" rhs 

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
