--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

{- |Functionality related to emitting a CapDL specification for initialising a
 -  system.
 -}
module OutputCapDL where

import qualified Data.Map as M
import Data.String.Utils (join)
import qualified Data.List as L
import Data.Elf (Elf)

import Control.Exception (assert)

import qualified CapDLObjects as CapDL
import Utils
import ELFFile
import Objects

-- |Creates a list of frame objects and caps to them (pages) for a given cell.
toCapDLPages :: (String, Elf) -> [SpecObject] -> [SpecObject] -> [Frame] -> M.Map Integer CapDL.Cap
toCapDLPages = toCapDLPages' 0

toCapDLPages' :: Integer -> (String, Elf) -> [SpecObject] -> [SpecObject] -> [Frame] -> M.Map Integer CapDL.Cap
toCapDLPages' _ _ _ _ [] = M.empty
toCapDLPages' index (name, elf) segs use_segs ((Frame vaddr r w x):xs) =
    assert (all isSegment segs) $
    assert (all isUseSegment use_segs) $
    case (lookupFrame elf segs use_segs vaddr) of
            -- Shared frame (note that we will actually create shared frames
            -- multiple times, but ignore this for now because they will be
            -- merged later):
        Just (segment_name, frame_index) -> M.insert vaddr y $ toCapDLPages' index (name, elf) segs use_segs xs
            where
                    -- Retrieve the use-segment entry for this cell that
                    -- matches the segment backing the shared frame:
                use_segs' = filter (\x -> useseg_name x == segment_name) use_segs
                use_seg = head $ assert (length use_segs' == 1) use_segs'
                    -- For a shared frame we want to only map it into this
                    -- cell's address space with the permissions given in the
                    -- input specification:
                r' = useseg_read use_seg
                w' = useseg_write use_seg
                x' = False
                y = CapDL.PageCap (CapDL.Frame $ "shared_frame_" ++ segment_name ++ "_" ++ (show frame_index)) r' w' x'
            -- Dedicated frame:
        Nothing -> M.insert vaddr y $ toCapDLPages' (index + 1) (name, elf) segs use_segs xs
            where
                y = CapDL.PageCap (CapDL.Frame $ "frame_" ++ name ++ "_" ++ (show index)) r w x

-- |Create the cap to a single PT from a PT index and map of pages.
getPT :: Arch -> String -> Integer -> M.Map Integer CapDL.Cap -> CapDL.Cap
getPT arch prefix index pages =
    CapDL.PTCap $ CapDL.PT ("pt_" ++ prefix ++ "_" ++ (show index)) $ M.filterWithKey (\k _ -> ptIndex arch k == index) pages

-- |Infer all containing PT caps from a map of pages.
getPTs :: Arch -> String -> M.Map Integer CapDL.Cap -> M.Map Integer CapDL.Cap
getPTs arch prefix pages =
    foldl (\m index -> M.insert index (getPT arch prefix index pages) m) M.empty $ L.nub $ map (ptIndex arch) $ M.keys pages

{- |Create the page directory for a given cell. We assume that each cell has
 -  only one PD.
 -}
getPD :: String -> M.Map Integer CapDL.Cap -> CapDL.Cap
getPD prefix pts =
    assert (all CapDL.isPTCap $ M.elems pts) $
    CapDL.PDCap $ CapDL.PD ("pd_" ++ prefix) pts

{- |Derive all the objects for a given cell. Note that all the caps of the
 -  system are derivable from this because every cap must be contained in an
 -  object.
 -}
toCapDL :: Arch -> (String, Elf) -> [SpecObject] -> [SpecObject] -> [Frame] -> [CapDL.Object]
toCapDL arch (name, elf) segments use_segments frames =
    assert (all isSegment segments) $
    assert (all isUseSegment use_segments) $
    objs
        where
            pages = M.filterWithKey (\k _ -> not $ isGuardPage elf k) $
                toCapDLPages (name, elf) segments use_segments frames
            pts = getPTs arch name pages
            pd = getPD name pts
            cspace = CapDL.CNodeCap $ CapDL.CNode $ "cnode_" ++ name
            tcb = CapDL.TCB ("tcb_" ++ name) (entryPoint elf) (stackPointer elf) name pd cspace
            caps = cspace : M.elems pages ++ M.elems pts ++ [pd]
            objs = tcb : map CapDL.getObj caps

{- |Transform a collection of objects into a CapDL spec. It is assumed that the
 -  objects may contain duplicates (multiple representations of the
 -  same object) that should be de-duped.
 -}
toCapDLSpec :: Arch -> [[CapDL.Object]] -> CapDL.Spec
toCapDLSpec arch xs =
    CapDL.Spec arch objs
        where
                -- Assume that a naïve join of the objects may encounter duplicates.
            objs = L.nub $ concat xs
