{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified    Data.Map        as M
import              Prelude         hiding (readFile, writeFile)
import              Text.Hamlet.XML
import              Text.XML
import              Data.Text
import              Data.Foldable
import              Text.Printf
import              Data.List

-- import qualified Data.Map.Strict as Map
-- import Data.Map (Map())

main :: IO [(String, String)]
main = do
    -- readFile will throw any parse errors as runtime exceptions
    Document prologue root epilogue <- readFile def "src_data/route_example_1.gpx"
    Document proFull rootFull epiFull <- readFile def "src_data/apple_health_export/workout-routes/route_2020-07-11_8.04pm.gpx"
    let resultingValues = procMain root

    putStrLn $ Data.List.unlines $ Data.List.map showDataLine resultingValues
    return(resultingValues)        

showDataLine :: (String, String) -> String
showDataLine (name, value) = printf "%7s = %s" name value

getNodeContent :: [Node] -> String
getNodeContent (x:xs) = unpack $ getN x 
    where 
        getN (NodeContent c) = c 

procElem :: Element -> [(String, String)] -> (Element, [(String, String)])
procElem (Element (Name elmName n2 n3) attrs children) xs 
    | elmName == pack "speed" = (emptyElement, ("Speed", value):xs)
    | elmName == pack "name" = (emptyElement, ("name", value):xs)
    | elmName == pack "time" = (emptyElement, ("time", value):xs)
    -- | elmName == pack "hAcc" = (emptyElement, ("hAcc", value):xs)
    -- | elmName == pack "vAcc" = (emptyElement, ("vAcc", value):xs)
    -- | elmName == pack "ele" = (emptyElement, ("ele", value):xs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNode c xs | c <- children]

procNode :: Node -> [(String, String)] -> ([Node], [(String, String)])
procNode (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElem e xs
procNode (NodeContent t) xs = ([NodeContent t], xs)
procNode (NodeComment _) _ = ([], [])           -- hide comments
procNode (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procMain :: Element -> [(String, String)]
procMain rootElement = dataResult
    where
        (_, dataResult) = procElem rootElement []

-- map with ints as keys and strings as values
myMap :: M.Map Int String
myMap = M.fromList [(5,"a"), (3,"b"), (5, "c")]
