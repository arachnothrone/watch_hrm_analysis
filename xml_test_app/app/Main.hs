{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE GADTs             #-}

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

data ValuePoint = ValuePoint 
    { paramName     :: String
    , paramValue    :: String
    } -- deriving (Show)

data DataPoint = DP TimeStamp Speed
--data DataPointHR = undefined

instance Show ValuePoint where
    show (ValuePoint parameter value) = printf "Parameter %7s: %s" parameter value

instance Show DataPoint where
    show (DP time speedValue) = printf "Time: %s, speed: %s" time speedValue

type TimeStamp  = String
type Speed      = String

main :: IO [DataPoint]
-- main :: IO [ValuePoint]
main = do
    -- readFile will throw any parse errors as runtime exceptions
    Document prologue root epilogue <- readFile def "src_data/route_example.gpx"
    Document proFull rootFull epiFull <- readFile def "src_data/apple_health_export/workout-routes/route_2020-07-11_8.04pm.gpx"
    --let resultingValues = procMain root
    let resultingValues = procMain' root


    -- putStrLn $ Data.List.unlines $ Data.List.map showDataLine resultingValues
    putStrLn $ Data.List.unlines $ Data.List.map show resultingValues
    return(resultingValues)

showDataLine :: ValuePoint -> String
showDataLine (ValuePoint name value) = printf "%7s = %s" name value

getNodeContent :: [Node] -> String
getNodeContent (x:xs) = unpack $ getN x 
    where 
        getN (NodeContent c) = c 

procElem :: Element -> [ValuePoint] -> (Element, [ValuePoint])
procElem (Element (Name elmName n2 n3) attrs children) xs 
    -- | elmName == pack "trkpt" = procElem 
    --                                 (Element "Trackpoint" (M.fromList []) $ Data.Foldable.concat processedChildren) $ 
    --                                 Data.Foldable.concat xs'
    | elmName == pack "trkpt" = procElemTrackpoint 
                                    (Element "Trackpoint" (M.fromList []) $ Data.Foldable.concat processedChildren) $ 
                                    Data.Foldable.concat xs'
    -- | elmName == pack "speed" = (emptyElement, (ValuePoint "Speed" value):xs)
    -- | elmName == pack "name" = (emptyElement, (ValuePoint "name" value):xs)
    -- | elmName == pack "time" = (emptyElement, (ValuePoint "time" value):xs)
    -- | elmName == pack "hAcc" = (emptyElement, ("hAcc", value):xs)
    -- | elmName == pack "vAcc" = (emptyElement, ("vAcc", value):xs)
    -- | elmName == pack "ele" = (emptyElement, ("ele", value):xs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNode c xs | c <- children]

procElem' :: Element -> [DataPoint] -> (Element, [DataPoint])
procElem' (Element (Name elmName n2 n3) attrs children) xs
    | elmName == pack "trkpt" = procElemTrackpoint' 
                                    (Element "Time" (M.fromList []) $ Data.Foldable.concat processedChildren) $ 
                                    Data.Foldable.concat xs'
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNode' c xs | c <- children]

procElemTrackpoint :: Element -> [ValuePoint] -> (Element, [ValuePoint])
procElemTrackpoint (Element (Name elmName n2 n3) attrs children) xs
    | elmName == pack "time" = (emptyElement, (ValuePoint "time" value):xs)
    | elmName == pack "speed" = (emptyElement, (ValuePoint "Speed" value):xs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNodeTrackpoint c xs | c <- children]

procElemTrackpoint' :: Element -> [DataPoint] -> (Element, [DataPoint])
procElemTrackpoint' (Element (Name elmName n2 n3) attrs children) xs
    | elmName == pack "time" = (emptyElement, (DP value "empty"):xs)
    | elmName == pack "speed" = (emptyElement, (DP ts value):xs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNodeTrackpoint' c xs | c <- children]
        (DP ts spd) = getLastDP xs
            -- | Data.List.null xs = DP "TS_null" "SPD_null"
            -- | otherwise = Data.List.last xs

getLastDP :: [DataPoint] -> DataPoint
getLastDP [] = DP "TS_null" "SPD_null"
getLastDP xs = Data.List.last xs

procNode :: Node -> [ValuePoint] -> ([Node], [ValuePoint])
procNode (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElem e xs
procNode (NodeContent t) xs = ([NodeContent t], xs)
procNode (NodeComment _) _ = ([], [])           -- hide comments
procNode (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procNode' :: Node -> [DataPoint] -> ([Node], [DataPoint])
procNode' (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElem' e xs
procNode' (NodeContent t) xs = ([NodeContent t], xs)
procNode' (NodeComment _) _ = ([], [])           -- hide comments
procNode' (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procNodeTrackpoint :: Node -> [ValuePoint] -> ([Node], [ValuePoint])
procNodeTrackpoint (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElemTrackpoint e xs
procNodeTrackpoint (NodeContent t) xs = ([NodeContent t], xs)
procNodeTrackpoint (NodeComment _) _ = ([], [])           -- hide comments
procNodeTrackpoint (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procNodeTrackpoint' :: Node -> [DataPoint] -> ([Node], [DataPoint])
procNodeTrackpoint' (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElemTrackpoint' e xs
procNodeTrackpoint' (NodeContent t) xs = ([NodeContent t], xs)
procNodeTrackpoint' (NodeComment _) _ = ([], [])           -- hide comments
procNodeTrackpoint' (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procMain :: Element -> [ValuePoint]
procMain rootElement = dataResult
    where
        (_, dataResult) = procElem rootElement []

procMain' :: Element -> [DataPoint]
procMain' rootElement = dataResult
    where
        (_, dataResult) = procElem' rootElement []

-- map with ints as keys and strings as values
myMap :: M.Map Int String
myMap = M.fromList [(5,"a"), (3,"b"), (5, "c")]
