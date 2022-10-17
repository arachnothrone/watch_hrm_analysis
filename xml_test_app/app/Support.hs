module Support where

import StringOp
import System.IO
import Data.Char(toUpper)
import Data.Maybe

main2 :: IO()
main2 = do
    -- inHndlr <- openFile "src_data/apple_health_export/export.xml" ReadMode  
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
    putStrLn "--> end no explicit loop"

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

getParamValue :: String -> String -> Maybe String
getParamValue parameter line
    | parExists == True = Just $ unquote '\"' rhs
    | otherwise = Nothing
    where
        (_, parExists, rhs) = splitAtSubstring parameter line
