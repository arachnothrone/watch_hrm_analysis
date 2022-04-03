{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML
import           Data.Text
import Data.Foldable

main :: IO ()
main = do
    -- readFile will throw any parse errors as runtime exceptions
    Document prologue root epilogue <- readFile def "src_data/route_example_1.gpx"

    -- -- root is the root element of the document, let's modify it
    -- let root' = transform root
    let rootListTups = getElevations root

    putStrLn "theend"

    

    -- -- And now we write out. Let's indent our output
    -- writeFile def
    --     { rsPretty = True
    --     } "output.html" $ Document prologue root' epilogue

--getElevations :: Element -> [(String, String)]     -- Tuple (timeStamp, elevation)
getElevations :: Element -> [[Node]]
getElevations (Element name attribs children) = [goNode child | child <- children]

-- -- We'll turn out <document> into an XHTML document
-- transform :: Element -> Element
-- transform (Element _name attrs children) = Element "html" M.empty
--     [xml|
--         <head>
--             <title>
--                 $maybe title <- M.lookup "title" attrs
--                     \#{title}
--                 $nothing
--                     Untitled Document
--         <body>
--             $forall child <- children
--                 ^{goNode child}
--      |]

-- elmTpl :: (Element, [String]) -> Element
-- elmTpl (a, _) = a 

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
-- goElem :: Element -> (Element, [String])
goElem (Element (Name elmName _ _) attrs children)
    | elmName == pack "metadata" = Element "--------------> metadata" attrs $ Data.Foldable.concatMap goNode children
    | elmName == pack "time" = Element "--------------> time" attrs $ Data.Foldable.concatMap goNode children
    -- | otherwise = let n = (unpack elmName ++ ": -----------> 000") in 
    --     Element 
    --         Name {
    --             nameLocalName = pack n, 
    --             nameNamespace = Nothing, 
    --             namePrefix = Nothing
    --         }
    --         attrs $ Data.Foldable.concatMap goNode children
    | otherwise = let n = (unpack elmName ++ ": -----------> 000") in
        Element (Name {pack n Nothing Nothing) attrs $ Data.Foldable.concatMap goNode children
-- goElem (Element "metadata" attrs children) = 
--     Element "--------------> metadata" attrs $ concatMap goNode children
-- goElem (Element "time" attrs children) = 
--     Element "--------------> time" attrs $ concatMap goNode children
-- goElem (Element "trk" attrs children) = 
--     Element "--------------> trk" attrs $ concatMap goNode children
-- goElem (Element "name" attrs children) = 
--     Element "--------------> name" attrs $ concatMap goNode children
-- goElem (Element "trkseg" attrs children) = 
--     Element "--------------> trkseg" attrs $ concatMap goNode children
-- goElem (Element "trkpt" attrs children) = 
--     Element "--------------> trkpt" attrs $ concatMap goNode children
-- goElem (Element "ele" attrs children) = 
--     Element "--------------> ele" attrs $ concatMap goNode children
-- goElem (Element "extensions" attrs children) = 
--     Element "--------------> extensions" attrs $ concatMap goNode children
-- goElem (Element "vAcc" attrs children) = 
--     Element "--------------> vAcc" attrs $ concatMap goNode children
-- goElem (Element "speed" attrs children) = 
--     Element "--------------> speed" attrs $ concatMap goNode children
-- goElem (Element "course" attrs children) = 
--     Element "--------------> course" attrs $ concatMap goNode children
-- goElem (Element "hAcc" attrs children) = 
--     Element "--------------> hAcc" attrs $ concatMap goNode children
-- goElem (Element "lon" attrs children) = 
--     Element "--------------> lon" attrs $ concatMap goNode children
-- goElem (Element "lat" attrs children) = 
--     Element "--------------> lat" attrs $ concatMap goNode children
-- -- goElem (Element "para" attrs children) =
-- --     Element "p" attrs $ concatMap goNode children
-- -- goElem (Element "em" attrs children) =
-- --     Element "i" attrs $ concatMap goNode children
-- -- goElem (Element "strong" attrs children) =
-- --     Element "b" attrs $ concatMap goNode children
-- -- goElem (Element "image" attrs _children) =
-- --     Element "img" (fixAttr attrs) [] -- images can't have children
-- --   where
-- --     fixAttr mattrs
-- --         | "href" `M.member` mattrs  = M.delete "href" $ M.insert "src" (mattrs M.! "href") mattrs
-- --         | otherwise                 = mattrs
-- goElem (Element name attrs children) =
--     -- don't know what to do, just pass it through...
--     Element name attrs $ concatMap goNode children

-- -- goElem (Element name attrs children) = 
-- --     Element "sdf" attrs []
