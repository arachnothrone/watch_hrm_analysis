-- support file
--
--
import System.IO
import Data.Char(toUpper)

main :: IO()
main = do
    inHndlr <- openFile "src_data/apple_health_export/export.xml" ReadMode
    outHndlr <- openFile "output.txt" AppendMode -- WriteMode
    inputStr <- hGetContents inHndlr
    hPutStr outHndlr (map toUpper inputStr)
    hClose inHndlr
    hClose outHndlr


