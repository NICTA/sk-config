--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

module ELFFile where

import Control.Exception (assert)
import Data.Bits (shiftR)
import Data.Elf -- elf
import qualified Data.List as L (nub)
import System.IO
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bs8
import GHC.Exts (the)
import Data.Word

import Objects
import Utils

(∈) = elem

-- |A frame derived from an ELF file.
data Frame = Frame {
    f_vaddr :: Integer,
    f_read :: Bool,
    f_write :: Bool,
    f_execute :: Bool
} deriving (Eq, Show)

-- |Opens an ELF file for reading based on a relative or absolute path.
openElf :: String -> IO Elf
openElf f = do
    handle <- openFile f ReadMode
    contents <- Bs.hGetContents handle
    return $ parseElf contents

-- |Returns the architecture of an open ELF file.
elfArch :: Elf -> Arch
elfArch Elf { elfMachine = EM_ARM } = Arm
elfArch Elf { elfMachine = EM_386 } = IA32
elfArch _ = assert False Unsupported

-- FRAME RETRIEVAL FUNCTIONS

-- |Reduces two (overlapping) frames to a single one with a union of their
-- permissions.
mergeFrames :: Frame -> Frame -> Frame
mergeFrames (Frame v1 r1 w1 x1) (Frame v2 r2 w2 x2) = assert (v1 == v2)
    (Frame v1 (r1 || r2) (w1 || w2) (x1 || x2))

-- |Appends a frame to a list, de-duping as we go.
appendFrame :: [Frame] -> Frame -> [Frame]
appendFrame [] f = [f]
appendFrame (x:xs) f
    | f_vaddr x == f_vaddr f = (mergeFrames x f):xs
    | otherwise = x:(appendFrame xs f)

-- |Joins two frame lists, de-duping as we go.
joinFrames :: [Frame] -> [Frame] -> [Frame]
joinFrames [] ys = ys
joinFrames (x:xs) ys = joinFrames xs (appendFrame ys x)

-- |Generates necessary frames to back a page-aligned region.
getSegmentFrames' :: Bool -> Bool -> Bool -> Integer -> Integer -> [Frame]
getSegmentFrames' r w x base size
    | (size == 0) = []
    | (size <= page_size) = assert (base % page_size == 0)
        [(Frame base r w x)]
    | otherwise = assert (base % page_size == 0)
        ((Frame base r w x):(getSegmentFrames' r w x (base + page_size) (size - page_size)))

-- |Maps a segment from an ELF file to a collection of frames required to back
-- it.
getSegmentFrames :: ElfSegment -> [Frame]
getSegmentFrames (ElfSegment PT_LOAD flags vaddress _ _ _ size) =
    -- Wrapper to expand the segment to page-aligned boundaries.
    getSegmentFrames' (PF_R ∈ flags) (PF_W ∈ flags) (PF_X ∈ flags) v_aligned (sz + v - v_aligned)
        where
            sz = fromIntegral size
            v = fromIntegral vaddress
            v_aligned = pageAlignDown v
getSegmentFrames _ = [] -- Ignore non-loadable segments.

-- |Retrieves all the frames we need to map for a given ELF file (note that
-- whether data actually needs to be loaded from the ELF file is ignored).
getFrames :: Elf -> [Frame]
getFrames Elf { elfSegments = segs } = foldl joinFrames [] (map getSegmentFrames segs)

-- |Returns the address of a symbol in the ELF file. Assumes the symbol is
-- unique. Note, this undoubtedly won't work on a stripped binary.
symbolAddress :: Elf -> String -> Integer
symbolAddress elf sym = fromIntegral $ steValue $ the $ filter (\x -> case (snd $ steName x) of
    (Just s) -> ((Bs8.unpack s) == sym)
    _ -> False) (concat $ parseSymbolTables elf)

{- |A page within a cell's address space can map either a shared or a dedicated
    frame. This can be determined by looking at the 'segment' and relevant
    'use-segment' information from the input specification. This function maps
    a given virtual address (of the start of a page) to a shared region name,
    optional physical address and index triple (recall that a shared region 
    can occupy more than one frame) or Nothing in the event that this address 
    maps a dedicated frame.
-}
lookupFrame :: Elf -> [SpecObject] -> [SpecObject] -> Integer -> Maybe (String, Maybe Integer, Integer)
lookupFrame _ _ [] _ = Nothing
lookupFrame elf segs (x:xs) vaddr =
    assert (all isSegment segs) $
    assert (isUseSegment x) $
    assert (vaddr % page_size == 0) $ do -- vaddr should be the start of a page.
    let seg = toSegment x segs
    let sym_addr = symbolAddress elf (useseg_alias x)
    if (vaddr >= sym_addr && vaddr < sym_addr + (seg_size seg))
            -- The current segment covers this frame.
        then Just (seg_name seg, seg_paddr seg, (vaddr - sym_addr) `div` page_size)
            -- The current segment and this frame are distinct.
        else lookupFrame elf segs xs vaddr

{- |Whether an address is the base of a guard page. Guard pages should be
 -  excluded from mapping when loading the ELF file. This allows checks for
 -  things like running off either end of your stack.
 -}
isGuardPage :: Elf -> Integer -> Bool
isGuardPage elf vaddr =
    assert (vaddr % page_size == 0) $
    vaddr == lower_stack_guard || vaddr == upper_stack_guard
        where
            stack_region = symbolAddress elf stack_symbol
            lower_stack_guard = stack_region
            upper_stack_guard = stack_region + page_size + stack_size

-- |Get the entry point of an ELF file.
entryPoint :: Elf -> Integer
entryPoint (Elf _ _ _ _ _ _ _ entry _ _) = fromIntegral entry

{- |Get the stack pointer, assuming the stack was generated by this tool. Note
 -  that we adjust up past the guard page and to the top of the (descending)
 -  stack.
 -}
stackPointer :: Elf -> Integer
stackPointer elf = symbolAddress elf stack_symbol + page_size + stack_size
