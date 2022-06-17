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
type CurrentTS  = String

main :: IO [DataPoint]
--main :: IO [ValuePoint]
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
getNodeContent [] = "gnc_null"
getNodeContent (x:xs) = unpack $ getN x
    where 
        getN (NodeContent c) = c 
        
-- getNodeContent' :: [Node] -> String


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

procElem' :: Element -> [DataPoint] -> CurrentTS -> (Element, [DataPoint], CurrentTS)
procElem' (Element (Name elmName n2 n3) attrs children) xs currentTs
    | elmName == pack "trkpt" = procElem' 
                                    (Element "TrackPoint" (M.fromList []) $ Data.Foldable.concat processedChildren)
                                    (Data.Foldable.concat xs')
                                    (currentTs)
    | elmName == pack "time" = (emptyElement, xs, value)
    | elmName == pack "speed" = (emptyElement, (DP currentTs value):xs, "reset_TS")
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs', extractTs currentTsList)
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        --(processedChildren, xs', currentTs') = unzip [procNode' c xs currentTs| c <- children]
        (processedChildren, xs', currentTsList) = munzip [procNode' c xs currentTs| c <- children]
        -- ((resElement, resCurrTs), resDPs) = procElemTrackpoint' 
        --                             (Element "Time" (M.fromList []) $ Data.Foldable.concat processedChildren) (
        --                             Data.Foldable.concat xs' )(
        --                             "emptyCurrentTS")

munzip :: [([Node], [DataPoint], CurrentTS)] -> ([[Node]], [[DataPoint]], [CurrentTS])
munzip = Prelude.undefined

extractTs :: [CurrentTS] -> CurrentTS
extractTs = Prelude.undefined


procElemTrackpoint :: Element -> [ValuePoint] -> (Element, [ValuePoint])
procElemTrackpoint (Element (Name elmName n2 n3) attrs children) xs
    | elmName == pack "time" = (emptyElement, (ValuePoint "time" value):xs)
    | elmName == pack "speed" = (emptyElement, (ValuePoint "Speed" value):xs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNodeTrackpoint c xs | c <- children]

procElemTrackpoint' :: Element -> [DataPoint] -> CurrentTS -> ((Element, CurrentTS), [DataPoint])
procElemTrackpoint' (Element (Name elmName n2 n3) attrs children) xs currTime
    -- | elmName == pack "time" = (nonEmptyElement, (DP value "empty"):xs)
    | elmName == pack "time" = ((emptyElement, value), xs)
    | elmName == pack "speed" = ((emptyElement, "-"), (DP currTime value):xs)
    | otherwise = ((Element (Name elmName n2 n3) attrs $ Data.Foldable.concat (fst (unzip resTupList)), currTime), Data.Foldable.concat xs')
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        --nonEmptyElement = Element (Name (pack "asdf___________________________") n2 n3) (M.fromList []) []
        (resTupList, xs') = unzip [procNodeTrackpoint' c xs value | c <- children]
            -- where
            --     (processedChildren, tss) = unzip resTupList
        -- -- (processedChildren, cTs), xs')

        --(DP ts spd) = getHeadDP xs
            -- | Data.List.null xs = DP "TS_null" "SPD_null"
            -- | otherwise = Data.List.last xs

getLastDP :: [DataPoint] -> DataPoint
getLastDP [] = DP "TS_null" "SPD_null"
getLastDP xs = Data.List.last xs

getHeadDP :: [DataPoint] -> DataPoint
getHeadDP [] = DP "TS_null" "SPD_null"
getHeadDP xs = Data.List.head xs

procNode :: Node -> [ValuePoint] -> ([Node], [ValuePoint])
procNode (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElem e xs
procNode (NodeContent t) xs = ([NodeContent t], xs)
procNode (NodeComment _) _ = ([], [])           -- hide comments
procNode (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procNode' :: Node -> [DataPoint] -> CurrentTS -> ([Node], [DataPoint], CurrentTS)
procNode' (NodeElement e) xs ts = ([NodeElement ge1], ge2, ts')
    where
        (ge1, ge2, ts') = procElem' e xs ts
procNode' (NodeContent t) xs ts = ([NodeContent t], xs, ts)
procNode' (NodeComment _) _ _ = ([], [], "")           -- hide comments
procNode' (NodeInstruction _) _ _ = ([], [], "")       -- hide processing instructions

procNodeTrackpoint :: Node -> [ValuePoint] -> ([Node], [ValuePoint])
procNodeTrackpoint (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElemTrackpoint e xs
procNodeTrackpoint (NodeContent t) xs = ([NodeContent t], xs)
procNodeTrackpoint (NodeComment _) _ = ([], [])           -- hide comments
procNodeTrackpoint (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procNodeTrackpoint' :: Node -> [DataPoint] -> CurrentTS -> (([Node], CurrentTS), [DataPoint])
procNodeTrackpoint' (NodeElement e) xs currTime = (([NodeElement ge1], ts), ge2)
    where
        ((ge1, ts), ge2) = procElemTrackpoint' e xs currTime
procNodeTrackpoint' (NodeContent t) xs ts = (([NodeContent t], ts), xs)
procNodeTrackpoint' (NodeComment _) _ _ = (([], ""), [])           -- hide comments
procNodeTrackpoint' (NodeInstruction _) _ _ = (([], ""), [])       -- hide processing instructions

procMain :: Element -> [ValuePoint]
procMain rootElement = dataResult
    where
        (_, dataResult) = procElem rootElement []

procMain' :: Element -> [DataPoint]
procMain' rootElement = dataResult
    where
        (_, dataResult, _) = procElem' rootElement [] "no_ts"

-- map with ints as keys and strings as values
myMap :: M.Map Int String
myMap = M.fromList [(5,"a"), (3,"b"), (5, "c")]
