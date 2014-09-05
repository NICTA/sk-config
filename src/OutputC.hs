--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

-- Functions for generating C source code from an input specification.
module OutputC (showHeader, showSource) where

import Control.Exception (assert)
import Data.Char (toUpper)
import Data.String.Utils (join) -- MissingH

import Objects
import Utils

-- |Function prototypes for a given region. Only valid when applied to a
-- UseSegment.
prototypes :: SpecObject -> String
prototypes use_seg = assert (isUseSegment use_seg) $
    unlines ["size_t region_" ++ alias ++ "_size(void);",
        "int region_" ++ alias ++ "_read(void *dest, size_t size, off_t offset);",
        "int region_" ++ alias ++ "_write(void *src, size_t size, off_t offset);",
        "void *region_" ++ alias ++ "_base(void);"]
        where
            alias = useseg_alias use_seg

fromChanProto :: SpecObject -> String
fromChanProto (Channel name _ _ _ _ _) = unlines ["write_channel_p " ++ name ++ ";"]

toChanProto :: SpecObject-> String
toChanProto (Channel name _ _ _ _ _) = unlines ["read_channel_p " ++ name ++ ";"]

-- |The contents of a C header for a given cell.
showHeader :: String -> [SpecObject] -> [SpecObject] -> String
showHeader cell_name use_segs channels = assert (all isUseSegment use_segs) $ indent 0 $
    unlines (["#ifndef _" ++ (map toUpper cell_name) ++ "_DRIVER_H_",
        "#define _" ++ (map toUpper cell_name) ++ "_DRIVER_H_",
        "",
        "#include <stddef.h>",
        "#include <sys/types.h>",
        "#include <channels.h>",
        "",
        "int cell_main(int argc, char **argv);",
        ""] ++
        (map prototypes use_segs) ++
        (map fromChanProto (fromChannels cell_name channels)) ++
        (map toChanProto (toChannels cell_name channels)) ++
        ["",
        "#endif /* !_" ++ (map toUpper cell_name) ++ "_DRIVER_H_ */"])

-- |The definition of an underlying shared memory region within a cell.
regionArray :: String -> Integer -> String
regionArray region_name size =
    "volatile char " ++ region_name ++ "[ROUND_UP(" ++ (show size) ++ ", PAGE_SIZE)] __attribute__((aligned(PAGE_SIZE))) __attribute__((externally_visible));\n"

stack :: String
stack =
    "char " ++ stack_symbol ++ "[PAGE_SIZE + ROUND_UP(" ++ (show stack_size) ++ ", PAGE_SIZE) + PAGE_SIZE] __attribute((aligned(PAGE_SIZE))) __attribute__((externally_visible));"

-- Various function definitions:
regionSize :: String -> Integer -> String
regionSize region_name size =
    unlines ["size_t region_" ++ region_name ++ "_size(void) {",
        "return " ++ (show size) ++ ";",
        "}"]
regionRead :: String -> Bool -> String
regionRead region_name readable =
    unlines (["int region_" ++ region_name ++ "_read(void *dest, size_t size, off_t offset) {"] ++
        (case readable of
            True -> ["memcpy(dest, (void*)" ++ region_name ++ " + offset, size);",
                    "return 0;"]
            _ -> ["return -1;"]) ++
        ["}"])
regionWrite :: String -> Bool -> String
regionWrite region_name writeable =
    unlines (["int region_" ++ region_name ++ "_write(void *src, size_t size, off_t offset) {"] ++
        (case writeable of
            True -> ["memcpy((void*)" ++ region_name ++ " + offset, src, size);",
                             "return 0;"]
            _ -> ["return -1;"]) ++
        ["}"])
regionBase :: String -> String
regionBase region_name =
    unlines ["void *region_" ++ region_name ++ "_base(void) {",
        "return (void*)" ++ region_name ++ ";",
        "}"]

fromChanInits c@(Channel name _ _ msgsize slots overwrite) = unlines([
    "static struct write_channel " ++ sname ++ ";",
    name ++ " = write_channel(&" ++ sname ++ ", " ++ buf ++ ", " ++ read ++ ", " ++ write ++ ", " ++ (show msgsize) ++ ", " ++ (show slots) ++ ", " ++ (makeOverwrite overwrite) ++ ");"])
    where
        makeOverwrite True = "CHAN_ALLOW_OVERWRITES"
        makeOverwrite False = "CHAN_NO_OVERWRITES"
        sname = "channel_" ++ name ++ "_mem"
        makemem seg = "(struct mem){.base = region_" ++ seg ++ "_base(), .size = region_" ++ seg ++ "_size()}"
        buf = makemem (channelBufferSegmentName c)
        read = makemem (channelReaderSegmentName c)
        write = makemem (channelWriterSegmentName c)

toChanInits c@(Channel name _ _ msgsize slots _) = unlines([
    "static struct read_channel " ++ sname ++ ";",
    name ++ " = read_channel(&" ++ sname ++ ", " ++ buf ++ ", " ++ read ++ ", " ++ write ++ ", " ++ (show msgsize) ++ ", " ++ (show slots) ++ ");"])
    where
        sname = "channel_" ++ name ++ "_mem"
        makemem seg = "(struct mem){.base = region_" ++ seg ++ "_base(), .size = region_" ++ seg ++ "_size()}"
        buf = makemem (channelBufferSegmentName c)
        read = makemem (channelReaderSegmentName c)
        write = makemem (channelWriterSegmentName c)

{- |A stub main function to call the user's main. On Linux some setup needs to
 -  be done so a stub function is generated to do this then calls the user's
 -  main. On seL4 we don't need to do any setup before the user's code runs.
 -}
mainStub :: String -> [SpecObject] -> String
mainStub cell_name channels =
    unlines (["int main(int argc, char **argv) {"] ++
    map fromChanInits (fromChannels cell_name channels) ++
    map toChanInits (toChannels cell_name channels) ++
    ["return cell_main(argc, argv);",
    "}"])

-- |Return all functions for a given region.
functions :: SpecObject -> SpecObject -> String
functions use_seg seg = assert (isUseSegment use_seg) $ assert (isSegment seg) $
    unlines [regionArray name size,
        regionSize name size,
        regionRead name readable,
        regionWrite name writeable,
        regionBase name]
        where
            name = useseg_alias use_seg
            readable = useseg_read use_seg
            writeable = useseg_write use_seg
            size = seg_size seg

-- |The contents of a C source file for a given cell.
showSource :: String -> [SpecObject] -> [SpecObject] -> [SpecObject] -> String
showSource cell use_segs segs channels = assert (all isUseSegment use_segs) $ assert (all isSegment segs) $ indent 0 $
    unlines ["#include <string.h>",
        "#include \"" ++ cell ++ "_driver.h\"",
        "",
        "#ifdef PAGE_SIZE",
        "    #undef PAGE_SIZE",
        "#endif",
        "#define PAGE_SIZE 4096",
        "",
        "#ifndef ROUND_UP",
        "    #define ROUND_UP(x, n) (((x) + (n) - 1) / (n) * (n))",
        "#endif",
        "",
        stack,
        "",
        mainStub cell channels,
        "",
        unlines $ map (\x -> functions x (toSegment x segs)) use_segs]
