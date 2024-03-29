{-
Timestamp parsing and operations
-}

module TimeParsOp where

import Parsing
import StringOp
import Text.Printf


data TimeStampObj = TS Year Month Day Hour Min Sec TZOffset
data TZOffset = TZO TzSign Hour Min

instance Show TimeStampObj where
    -- show (TS year month day hour min sec tsOffs) = printf "%4d-%2d-%2d %2d:%2d:%2d UTC%d" year month day hour min sec tsOffs
    show (TS year month day hour min sec tsOffs) = printf "%4d-%2d-%2d %2d:%2d:%2d %s" year month day hour min sec $ show tsOffs

instance Show TZOffset where
    show (TZO sign hour min) = printf "UTC%c%02d:%02d" sign hour min

type Year       = Int
type Day        = Int
type Month      = Int
type Hour       = Int
type Min        = Int
type Sec        = Int
--type TZOffset   = Int
type TzSign     = Char

startYear = 2020
secondsInOneMin = 60 -- :: Integer
secondsInOneHour = 60 * secondsInOneMin -- :: Integer
secondsInOneDay = (24 * secondsInOneHour) -- :: Integer
secondsInOneMonth mnt leap = (getMntDays mnt leap * secondsInOneDay) -- :: Integer
secondsInOneYear year = foldl (+) 0 [secondsInOneMonth mntNum $ isYearLeap year | mntNum <- [1..12]]
mntMappingReg   = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
mntMappingLeap  = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


-- "2021-11-10 13:18:33 -0500"
readTimeStamp :: String -> Maybe TimeStampObj
readTimeStamp s = case parse parseTimeStamp2 s of
    Just (tsobj, "")    -> Just tsobj
    _                   -> Nothing 

readTimeStamp3 :: String -> Maybe TimeStampObj
readTimeStamp3 s 
    | exist1 && exist2 = Just $ TS (read year) (read month) (read day) (read hour) (read min) (read sec) (TZO sgn (read [h1, h2]::Hour) (read [m1, m2]::Min)) 
    | otherwise = Nothing
    where
        (dateString, exist1, rhs) = splitAtChar ' ' s
        (year, month, day) = getDateTime dateString '-'
        (timeString, exist2, offsetString) = splitAtChar ' ' rhs
        (hour, min, sec) = getDateTime timeString ':'
        (sgn:h1:h2:m1:m2:_) = offsetString

getDateTime :: String -> Char -> (String, String, String)
getDateTime s delimiter
    | exist1 && exist2 = (value1, value2, value2)
    | otherwise = ("", "", "")
    where
        (value1, exist1, rhs) = splitAtChar delimiter s
        (value2, exist2, value3) = splitAtChar delimiter rhs

convertTsToAbsSeconds :: Maybe TimeStampObj -> Int
convertTsToAbsSeconds Nothing = 0
convertTsToAbsSeconds (Just ts) = 
      secondsCumulativeToTheYear year
    + secondsCumulativeToTheMonth month (isYearLeap year)
    + secondsCumulativeToTheThing secondsInOneDay day
    + secondsCumulativeToTheThing secondsInOneHour hour
    + secondsCumulativeToTheThing secondsInOneMin min
    + sec 
        where
            (TS year month day hour min sec (TZO tzsn0 tzhr0 tzmin0)) = ts

parseTimeStamp2 :: Parser TimeStampObj
parseTimeStamp2 = do
    year <- natNumber
    char '-'
    mont <- natNumber
    char '-'
    day <- natNumber
    char ' '
    hh <- natNumber
    char ':'
    mm <- natNumber
    char ':'
    ss <- natNumber
    -- char ' '
    -- item 
    tzoff <- parseTzOffset
    return $ TS year mont day hh mm ss $ tzoff

parseTzOffset :: Parser TZOffset
parseTzOffset = do 
    char ' '
    sign <- sat (`elem` ['+', '-'])
    h1 <- digit
    h2 <- digit
    m1 <- digit
    m2 <- digit
    return $ TZO sign (read [h1, h2]::Hour) (read [m1, m2]::Min)

natNumber :: Parser Int
natNumber = do 
  ds <- oneOrMore digit
  return $ read ds

isYearLeap :: Int -> Bool
isYearLeap year = case year `mod` 4 of
    0   -> case year `mod` 100 of
        0   -> case year `mod` 400 of 
            0   -> True
            _   -> False
        _   -> True
    _   -> False

getMntDays mntNum yLeap = case yLeap of
    False   -> mntMappingReg !! (mntNum - 1)
    True    -> mntMappingLeap !! (mntNum - 1)

secondsCumulativeToTheYear :: Int -> Int
secondsCumulativeToTheYear y = foldl (\acc x -> ((+ acc) (secondsInOneYear x))) 0 [0..y - 1]

secondsCumulativeToTheMonth :: Int -> Bool -> Int
secondsCumulativeToTheMonth m bLeap = foldl (\acc x-> ((+ acc) (secondsInOneMonth x bLeap))) 0 [1..m - 1]

-- secondsCumulativeToTheDay :: Int -> Int
-- secondsCumulativeToTheDay d = secondsInOneDay * (d - 1)

-- secondsCumulativeToTheHour :: Int -> Int
-- secondsCumulativeToTheHour h = secondsInOneHour * (h - 1)

secondsCumulativeToTheThing :: Int -> Int -> Int
secondsCumulativeToTheThing f nr = f * (nr - 1)

