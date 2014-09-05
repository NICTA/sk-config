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
    Generator-specific types.
-}
module Objects where

import Control.Exception (assert)
import Data.String.Utils (strip) -- MissingH

import Utils

-- |An element derived from the XML input specification.
data SpecObject =
    Segment {
        seg_name :: String,
        seg_size :: Integer
    } -- Backed by one or more frames.
    | Cell {
        cell_name :: String,
        cell_rate :: Integer,
        cell_segs :: [SpecObject]
    } -- A component (single ELF file).
    | Channel {
        name :: String,
        cell_from :: String,
        cell_to   :: String,
        msgsize :: Integer,
        slots :: Integer,
        overwrite :: Bool
    }
    | UseSegment {
        useseg_name :: String,
        useseg_alias :: String,
        useseg_read :: Bool,
        useseg_write :: Bool
    } -- Mapping of a segment for a given cell.
    deriving (Eq, Ord, Show)

isUseSegment :: SpecObject -> Bool
isUseSegment UseSegment {} = True
isUseSegment _ = False

isSegment :: SpecObject -> Bool
isSegment Segment {} = True
isSegment _ = False

{- |Unravels a VM mapping. To put this another way, this function takes a
    'use-segment' (a shared frame mapping) to a 'segment' (its underlying
    frame).
-}
toSegment :: SpecObject -> [SpecObject] -> SpecObject
toSegment x segs = assert (isUseSegment x && all isSegment segs)
    head $ filter (\y -> useseg_name x == seg_name y) segs

-- Naming and helps for channels
chanSegName :: SpecObject -> String -> String
chanSegName (Channel name _ _ _ _ _ ) b = "channel_" ++ name ++ "_" ++ b ++ "_segment"

channelBufferSegmentName :: SpecObject -> String
channelBufferSegmentName c = chanSegName c "buffer"

channelReaderSegmentName :: SpecObject -> String
channelReaderSegmentName c = chanSegName c "reader"

channelWriterSegmentName :: SpecObject -> String
channelWriterSegmentName c = chanSegName c "writer"

fromChannels :: String -> [SpecObject] -> [SpecObject]
fromChannels from_name channels = filter(\(Channel _ from _ _ _ _) -> from == from_name) channels
toChannels :: String -> [SpecObject] -> [SpecObject]
toChannels to_name channels = filter(\(Channel _ _ to _ _ _) -> to == to_name) channels
