--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

{-
    A module for hiding some of the less pleasant bits of Text.XML.Light such
    as QNames. Also includes functions for transforming XML types into types
    specific to this generator.
-}

module XMLWrapper (readXML,
                   getRoot,
                   getElement,
                   getJustAttr,
                   getJustElement,
                   getElements,
                   getChild,
                   getJustChild,
                   getChildren,
                   parseSegment,
                   parseUseSegment,
                   parseCell) where

import Control.Exception (assert)
import System.IO
import Text.XML.Light -- xml

import qualified Numeric (readHex)

import Objects

-- |Read XML content from a file.
readXML :: String -> IO [Content]
readXML f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle
    xml <- return (parseXML contents)
    return xml
    -- Don't hClose because hGetContents is lazy.

-- |Return the uppermost (containing) element in an XML document.
getRoot :: [Content] -> Element
getRoot = head . onlyElems

-- Wrappers to avoid dealing with QNames and Maybes.
qname :: String -> QName
qname s = QName s Nothing Nothing

getElement :: Element -> String -> Maybe Element
getElement root name = findElement (qname name) root

getJustElement :: Element -> String -> Element
getJustElement root name = case (getElement root name) of
    Just e -> e
    Nothing -> error ("Element " ++ name ++ " not found")

getElements :: Element -> String -> [Element]
getElements root name = findElements (qname name) root

getChild :: Element -> String -> Maybe Element
getChild root name = findChild (qname name) root

getJustChild :: Element -> String -> Element
getJustChild root name = case (getChild root name) of
    Just e -> e
    Nothing -> error ("Element " ++ name ++ " not found")

getChildren :: Element -> String -> [Element]
getChildren root name = findChildren (qname name) root

getAttr :: Element -> String -> Maybe String
getAttr elem name = findAttr (qname name) elem

getJustAttr :: Element -> String -> String
getJustAttr elem name = case (getAttr elem name) of
    Just s -> s
    Nothing -> error ("Attribute " ++ name ++ " not found")

-- Generator-specific functions:

readHexStr :: String -> Maybe Integer
readHexStr s = 
  let str = (if take 2 s == "0x" then drop 2 s else s) in
    (case Numeric.readHex str of (i,bla):rs -> if bla == "" && rs == [] then Just i else Nothing
                                 [] -> Nothing)

parseSegment :: Element -> SpecObject
parseSegment e = assert (elName e == qname "segment")
    Segment (getJustAttr e "name") (read $ getJustAttr e "size") 
            (case (getAttr e "paddr") of 
                Nothing -> Nothing
                Just s -> readHexStr s)

parseUseSegment :: Element -> SpecObject
parseUseSegment e = assert (elName e == qname "use-segment")
    UseSegment (getJustAttr e "name") (getJustAttr e "alias")
        ((getJustAttr e "privileges") `elem` ["rw", "ro"]) -- read = read-write || read-only
        ((getJustAttr e "privileges") `elem` ["rw", "wo"]) -- write = read-write || write-only

parseCell :: Element -> SpecObject
parseCell e = assert (elName e == qname "cell")
    Cell (getJustAttr e "name") (read $ getJustAttr e "scheduleRate") (map parseUseSegment (getElements e "use-segment"))
