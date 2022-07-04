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
import              Data.Maybe
import              Data.String
import              Data.List.Split         -- splitOn

-- import qualified Data.Map.Strict as Map
-- import Data.Map (Map())

data ValuePoint = ValuePoint 
    { paramName     :: String
    , paramValue    :: String
    } -- deriving (Show)

data DataPoint = DP TimeStamp Speed | GPS Latitude Longitude | HD RecordType TimeStamp Value
--data DataPointHR = undefined

instance Show ValuePoint where
    show (ValuePoint parameter value) = printf "Parameter %7s: %s" parameter value

instance Show DataPoint where
    show (DP time speedValue) = printf "Time: %s, speed: %s" time speedValue
    show (GPS latitude longitude) = printf "Lat: %s, Lon: %s" latitude longitude
    show (HD record time value) = printf "%s: %s = %s" time record value

type TimeStamp  = String
type Speed      = String
type CurrentTS  = String
type Latitude   = String
type Longitude  = String
type Value      = String
type RecordType = String -- -> BMI | HR (heart rate)| ...

main :: IO ()
-- main :: IO [DataPoint]
--main :: IO [ValuePoint]
main = do
    -- readFile will throw any parse errors as runtime exceptions
    Document prologue root epilogue <- readFile def "src_data/route_example.gpx"
    Document proFull rootFull epiFull <- readFile def "src_data/apple_health_export/workout-routes/route_2020-07-11_8.04pm.gpx"
    Document prologueHData rootHData epilogueHData <- readFile def  "src_data/healthdata_example.xml" -- "src_data/healthdata_example.xml" -- "src_data/apple_health_export/export.xml"
    --let resultingValues = procMain root
    let resultingValues = procMain'' root
    let resultHealthData = procHealthData rootHData

    -- putStrLn $ Data.List.unlines $ Data.List.map showDataLine resultingValues
    putStrLn $ Data.List.unlines $ Data.List.map show resultingValues
    putStrLn $ Data.List.unlines $ Data.List.map show resultHealthData
    appendFile "export/testfile.txt" "33333------essss\n"
    return () -- (resultingValues)

showDataLine :: ValuePoint -> String
showDataLine (ValuePoint name value) = printf "%7s = %s" name value

dataPointString :: DataPoint -> String
dataPointString (HD record time value) = printf "%s, %s\n" time value

mntMappingReg   = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
mntMappingLeap  = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

getMntDays mntNum yLeap = case yLeap of
    False   -> mntMappingReg !! (mntNum - 1)
    True    -> mntMappingLeap !! (mntNum - 1)

isYearLeap :: Int -> Bool
isYearLeap year = case year `mod` 4 of
    0   -> case year `mod` 100 of
        0   -> case year `mod` 400 of 
            0   -> True
            _   -> False
        _   -> True
    _   -> False

startYear = 2020
secondsInOneMin = 60
secondsInOneHour = 60 * secondsInOneMin
secondsInOneDay = 24 * secondsInOneHour
secondsInOneMonth mnt leap = getMntDays mnt leap * secondsInOneDay
secondsInOneYear year = Data.Foldable.foldl (+) 0 [secondsInOneMonth mntNum $ isYearLeap year | mntNum <- [1..12]]

-- 2021-11-10 13:18:33 -0500
convertTimeToSec :: String -> Maybe Int
convertTimeToSec tsString = do
    let splitstr1 = Data.List.Split.splitOn " " tsString     -- Data.List.Split.
    -- dateList <- Data.List.Split.splitOn "-" splitstr1!!0
    -- timeList <- Data.List.Split.splitOn ":" splitstr1!!1
    return (dateSeconds + timeSeconds)
        where 
            dateSeconds = Prelude.undefined
            timeSeconds = Prelude.undefined

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
                                    (extractTs currentTsList2)
    -- | elmName == pack "trkpt" = procElem' 
    --                                 (Element "TrackPoint" (M.fromList []) $ children)
    --                                 (Data.Foldable.concat xs')
    --                                 (extractTs currentTsList2)
    | elmName == pack "time" = (emptyElement, xs, value)
    -- | elmName == pack "time" = procElem' 
    --                                 (Element "TrackPoint" (M.fromList []) $ Data.Foldable.concat processedChildren2)
    --                                 xs -- (Data.Foldable.concat xs')
    --                                 (value)
    | elmName == pack "speed" = (emptyElement, (DP currentTs value):xs, "")
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildrenOtherw, Data.Foldable.concat xsOth, extractTs currentTsListOth)
    where
        value = getNodeContent children
        emptyElement = Element "" (M.fromList []) []
        -- (processedChildren, xs', currentTs') = unzip [procNode' c xs currentTs| c <- children]
        (processedChildrenOtherw, xsOth, currentTsListOth) = munzip [procNode' c xs currentTs| c <- children]
        (processedChildren, xs', currentTsList)            = munzip [procNode' c xs (extractTs currentTsList2)| c <- children]
        -- ((resElement, resCurrTs), resDPs) = procElemTrackpoint' 
        --                             (Element "Time" (M.fromList []) $ Data.Foldable.concat processedChildren) (
        --                             Data.Foldable.concat xs' )(
        --                             "emptyCurrentTS")
        (_, _, currentTsList2) = munzip [procNode' c xs currentTs| c <- children]

procElem'' :: Element -> [DataPoint] -> (Element, [DataPoint])
procElem'' (Element (Name elmName n2 n3) attrs children) xs 
    | elmName == pack "trkpt" = (elementTP, dataPointsTP ++ dataPointsTP_attrs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNode'' c xs | c <- children]
        --(processedChildrenTP, xsTP, tsTPs) = munzip [procNodeTP'' c xs "start" | c <- children] -- fold .....
        (processedChildrenTP, xsTP, tsTP, _) = procNodesTP'' children xs "start"
        (elementTP, dataPointsTP, ttt) = procElemTP'' 
                                    (Element "TrackPoint" (M.fromList []) $ processedChildrenTP)
                                    (xsTP)
                                    (tsTP)
        dataPointsTP_attrs = processAttributes $ M.toList attrs

procElemHD :: Element -> [DataPoint] -> (Element, [DataPoint])
procElemHD (Element (Name elmName n2 n3) attrs children) xs 
    -- | elmName == pack "Record" = (emptyElement, (HD recType recTs recVal):xs)
    | elmName == pack "Record" = (emptyElement, newDataPoints)
    | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildren, Data.Foldable.concat xs')
    where
        -- (_, (recType, recTs, recVal)) = procAttribHD (M.toList attrs, ("", "", ""))
        emptyElement = Element "" (M.fromList []) []
        (processedChildren, xs') = unzip [procNodeHD c xs | c <- children]
        -- (_, dpResult) = procAttribHD (M.toList attrs, Just ("", "", ""))
        dpResult = procAttribHD attrs
        newDataPoints = case dpResult of
            Just (recType, recTs, recVal) -> (HD (unpack recType) (unpack recTs) (unpack recVal)):xs
            Nothing                       -> xs

-- procAttribHD :: ([(Name, Text)], Maybe (RecordType, TimeStamp, Value)) -> ([(Name, Text)], Maybe (RecordType, TimeStamp, Value))
-- procAttribHD ([], result) = ([], result)
-- procAttribHD (((Name a b c, currValue):xs), Just (rectype, ts, val))
--     | a == pack "type" && currValue == pack "HKQuantityTypeIdentifierBodyMassIndex" = procAttribHD_record_bmi(xs, Just ("bmi", ts, val))
--     -- | rectype == "bmi" = (xs, Just (rectype, ts, val))
--     -- | a == pack "startDate" = procAttribHD(xs, Just (rectype, Data.Text.unpack currValue, val))
--     -- | rectype == "bmi" && a == pack "value" = procAttribHD(xs, Just (rectype, ts, Data.Text.unpack currValue))
--     | otherwise = ([], Nothing) -- procAttribHD(xs, Just (rectype, ts, val))

findRecord :: (Maybe RecordType, Maybe TimeStamp, Maybe Value) -> Maybe (RecordType, TimeStamp, Value)
findRecord (Just a, Just b, Just c) = Just (a, b, c)
findRecord (_, _, _) = Nothing

--procAttribHD :: (Ord k, Data.String.IsString k) => M.Map k c -> Maybe (c, c, c)
--procAttribHD :: M.Map k c -> Maybe (c, c, c)
procAttribHD :: (Ord k, IsString k) => M.Map k Text -> Maybe (Text, Text, Text)
procAttribHD attrs = do
            rectyp  <- M.lookup "type" attrs
            -- rectype <- if (rectyp == pack "HKQuantityTypeIdentifierBodyMassIndex") then (Just rectyp) else Nothing
            -- rectype <- if (rectyp == pack "HKQuantityTypeIdentifierBodyMassIndex") then (Just $ pack "bmi") else Nothing
            rectype <- if (rectyp == pack "HKQuantityTypeIdentifierHeartRate") then (Just $ pack "hr") else Nothing
            recTs   <- M.lookup "startDate" attrs
            recVal  <- M.lookup "value" attrs
            return (rectype, recTs, recVal)

procAttribHD_record_bmi :: ([(Name, Text)], Maybe (RecordType, TimeStamp, Value)) -> ([(Name, Text)], Maybe (RecordType, TimeStamp, Value))
procAttribHD_record_bmi ([], result) = ([], result)
procAttribHD_record_bmi (((Name a b c, currValue):xs), Just (rectype, ts, val))
    | a == pack "startDate" = procAttribHD_record_bmi(xs, Just (rectype, Data.Text.unpack currValue, val))
    | a == pack "value" = procAttribHD_record_bmi(xs, Just (rectype, ts, Data.Text.unpack currValue))
    | otherwise = procAttribHD_record_bmi(xs, Just (rectype, ts, val))


processAttributes :: [(Name, Text)] -> [DataPoint]
-- processAttributes :: M.Map -> [DataPoint]
processAttributes [] = []
processAttributes ((Name a b c, value):xs) = (GPS (Data.Text.unpack a) (Data.Text.unpack value)) : processAttributes xs 
-- processAttributes attrs = 

procNode'' :: Node -> [DataPoint] -> ([Node], [DataPoint])
--procNode'' = Prelude.undefined
procNode'' (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElem'' e xs
procNode'' (NodeContent t) xs = ([NodeContent t], xs)
procNode'' (NodeComment _) _ = ([], [])           -- hide comments
procNode'' (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procElemTP'' :: Element -> [DataPoint] -> CurrentTS -> (Element, [DataPoint], CurrentTS)
procElemTP'' (Element (Name elmName n2 n3) attrs childNodes) xs currTime
    | elmName == pack "time" = (emptyElement, xs, value)
    | elmName == pack "speed" = (emptyElement, (DP currTime value):xs, "closed")
    -- | otherwise = (emptyElement, xs, currTime)
    -- | otherwise = (Element (Name elmName n2 n3) attrs $ Data.Foldable.concat processedChildrenTP, Data.Foldable.concat xsTP, extractTs tsTPs)
    | otherwise = (Element (Name elmName n2 n3) attrs $ processedChildrenTP, xsTP, tsTPs)
    where
        emptyElement = Element "" (M.fromList []) []
        value = getNodeContent childNodes
        -- (processedChildrenTP, xsTP, tsTPs) = munzip [procNodeTP'' c xs currTime | c <- childNodes]
        (processedChildrenTP, xsTP, tsTPs, _) = procNodesTP'' childNodes xs currTime

procNodeTP'' :: Node -> [DataPoint] -> CurrentTS -> ([Node], [DataPoint], CurrentTS)
procNodeTP'' (NodeElement e) xs ts = ([NodeElement resElement], resDps, resTs)
    where
        (resElement, resDps, resTs) = procElemTP'' e xs ts
procNodeTP'' (NodeContent t) xs ts = ([NodeContent t], xs, ts)
procNodeTP'' (NodeComment _) _ _ = ([], [], "")
procNodeTP'' (NodeInstruction _) _ _ = ([], [], "")

procNodesTP'' :: [Node] -> [DataPoint] -> CurrentTS -> ([Node], [DataPoint], CurrentTS, [Node])
procNodesTP'' [] dps ts = ([], dps, ts, [])
procNodesTP'' (nd:nds) dps ts = procNodesTP'' nds dps2 ts2 
    where
        (nds2, dps2, ts2) = procNodeTP'' nd dps ts 





-------------------------------------------------------------------------------------------
munzip :: [([Node], [DataPoint], CurrentTS)] -> ([[Node]], [[DataPoint]], [CurrentTS])
munzip [] = ([], [], [])
munzip (x:xs) = (a:al, b:bl, c:cl)
    where
        (a, b, c) = x
        (al, bl, cl) = munzip xs

zunzip :: [(Int, String, Char)] -> ([Int], [String], [Char])
zunzip [] = ([], [], [])
zunzip (x:xs) = (a:al, b:bl, c:cl)
    where
        (a, b, c) = x
        (al, bl, cl) = zunzip xs

extractTs :: [CurrentTS] -> CurrentTS
--extractTs = Prelude.undefined -- "no_ts"
--extractTs [] = ""
extractTs xs = Data.Foldable.concat [x | x <- xs]


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

procNodeHD :: Node -> [DataPoint] -> ([Node], [DataPoint])
procNodeHD (NodeElement e) xs = ([NodeElement ge1], ge2)
    where
        (ge1, ge2) = procElemHD e xs
procNodeHD (NodeContent t) xs = ([NodeContent t], xs)
procNodeHD (NodeComment _) _ = ([], [])           -- hide comments
procNodeHD (NodeInstruction _) _ = ([], [])       -- hide processing instructions

procMain :: Element -> [ValuePoint]
procMain rootElement = dataResult
    where
        (_, dataResult) = procElem rootElement []

procMain' :: Element -> [DataPoint]
procMain' rootElement = dataResult
    where
        (_, dataResult, _) = procElem' rootElement [] "no_ts"

procMain'' :: Element -> [DataPoint]
procMain'' rootElement = dataResult
    where
        (_, dataResult) = procElem'' rootElement []

procHealthData :: Element -> [DataPoint]
procHealthData rootElement = dataResult
    where
        (_, dataResult) = procElemHD rootElement []

-- map with ints as keys and strings as values
myMap :: M.Map Int String
myMap = M.fromList [(5,"a"), (3,"b"), (5, "c")]
