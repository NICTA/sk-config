--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

{- |CapDL types.
 -}
module CapDLObjects where

import Data.Map (Map, elems, foldWithKey, fromList, singleton)
import qualified Numeric (showHex)

import Utils

showHex x = Numeric.showHex x ""

{- |A kernel-level object, as represented in CapDL. Note that this type does not
 -  include objects that are unnecessary for the SK translation.
 -}
data Object =
    CNode { -- A capability store
        cnode_name :: String
            -- Note, we don't support non-empty CNodes.
    }
    | Frame { -- Frame of physical memory
        frame_name :: String,
        frame_paddr :: Maybe Integer
    }
    | PD { -- Page directory
        pd_name :: String,
            -- PTs keyed by their index in the containing PD.
        pd_pts  :: Map Integer Cap
    }
    | PT { -- Page table
        pt_name  :: String,
            -- Pages keyed by their base virtual address (NOT index in the
            -- containing PT).
        pt_pages :: Map Integer Cap
    }
    | TCB { -- Thread control block
        tcb_name   :: String,
        tcb_ip     :: Integer, -- Starting instruction pointer
        tcb_sp     :: Integer, -- Starting stack pointer
        tcb_elf    :: String, -- Name of associated image
        tcb_domain :: Int,
        tcb_vspace :: Cap,
        tcb_cspace :: Cap
    }
instance Eq Object where
    CNode n1 == CNode n2 = n1 == n2
    Frame n1 popt1 == Frame n2 popt2 = n1 == n2
    PD n1 _ == PD n2 _ = n1 == n2
    PT n1 _ == PT n2 _ = n1 == n2
    TCB n1 _ _ _ _ _ _ == TCB n2 _ _ _ _ _ _ = n1 == n2
    _ == _ = False
instance Show Object where
    show (CNode name) = name ++ " = cnode (4 bits)" -- Assume 4 bit CNodes
    show (Frame name popt) = name ++ " = frame (4k" ++ 
            (case popt of Just p -> ", paddr: 0x" ++ (showHex p) 
                          Nothing -> "") 
            ++ ")" -- Assume 4K frames
    show (PD name _) = name ++ " = pd"
    show (PT name _) = name ++ " = pt"
    show (TCB name ip sp elf dom _ _) = name ++ " = tcb (addr: 0x0, ip: 0x" ++
        (showHex ip) ++ ", sp: 0x" ++ (showHex sp) ++ ", elf: " ++ elf ++
        ", dom: " ++ (show dom) ++ ", prio: 125)"
        -- Note, for the systems we are targeting the user makes no syscalls
        -- and hence requires no IPC buffer. Also priority is irrelevant as
        -- each thread is in a domain by itself.

{- |Objects effectively have two show instances, one to show the object as it
 -  would appear in the "objects" section of a CapDL spec and one to show the
 -  contained capabilities as they would appear in the "caps" section of a
 -  CapDL spec. This is the latter.
 -}
showCaps :: Arch -> Object -> String
showCaps _ (PD name pts) =
    unlines $ (name ++ " {") :
        (foldWithKey (\vaddr cap acc -> acc ++ ["0x" ++ (showHex vaddr) ++ ": " ++ (show cap)]) [] pts) ++
        ["}"]
showCaps arch (PT name pages) =
    unlines $ (name ++ " {") :
        (foldWithKey (\vaddr cap acc -> acc ++ ["0x" ++ (showHex $ pageIndex arch vaddr) ++ ": " ++ (show cap)]) [] pages) ++
        ["}"]
showCaps _ (TCB name _ _ _ _ vspace cspace) =
    unlines $ (name ++ " {") :
        ("vspace: " ++ (show vspace)) :
        ("cspace: " ++ (show cspace)) :
        ["}"]
        -- Note, for the systems we are targeting the user makes no syscalls
        -- and hence needs no IPC buffer frame cap or CSpace. We also don't
        -- configure a fault handler.
showCaps _ _ = error "showCaps called on an object that is not a cap container"

{- |Whether an object is of a type that contains references to (child)
 -  capabilities.
 -}
isContainer :: Object -> Bool
isContainer (PD {}) = True
isContainer (PT {}) = True
isContainer (TCB {}) = True
isContainer _ = False

{- |A capability to a kernel-level object. As for Object, this type only covers
 _  the capabilities required for the SK translation.
 -}
data Cap =
    CNodeCap { -- CNode capability
        c_obj :: Object
    }
    | PageCap { -- Page (virtual memory mapping of a frame)
        p_obj     :: Object,
        p_read    :: Bool,
        p_write   :: Bool,
        p_execute :: Bool,
        p_uncached  :: Bool
    }
    | PTCap { -- Page table
        pt_obj :: Object
    }
    | PDCap { -- Page directory
        pd_obj :: Object
    }
instance Show Cap where
    show (CNodeCap obj) = cnode_name obj ++ " (guard: 0, guard_size: 28)" -- Assume some sensible guard
    show (PageCap obj r w x uc) = frame_name obj ++ " (" ++ (r ? "R" $ "") ++ (w ? "W" $ "") ++ (x ? "X" $ "") ++  (uc ? ", uncached" $ "") ++ ")"
    show (PTCap obj) = pt_name obj
    show (PDCap obj) = pd_name obj

-- |Retrieve the object that a capability points to.
getObj :: Cap -> Object
getObj (CNodeCap obj) = obj
getObj (PageCap obj _ _ _ _) = obj
getObj (PTCap obj) = obj
getObj (PDCap obj) = obj

{- |A CapDL textual representation. Note that the capabilities in a specification
 -  do not need to be represented directly because they are all contained by an
 -  object in the specification.
 -}
data Spec = Spec {
    spec_arch :: Arch,
    spec_objs :: [Object]
}
instance Show Spec where
    show (Spec arch objs) = indent 0 $
        unlines
                -- Arch header:
            ["arch " ++ (show arch),
            "",
                -- Objects:
            "objects {",
            (unlines $ map show objs) ++ "}",
            "",
                -- Capabilities:
            "caps {",
            (unlines $ map (showCaps arch) $ filter isContainer objs) ++ "}"]

-- Helpers used in assertions.
isFrame :: Object -> Bool
isFrame (Frame {}) = True
isFrame _ = False
isPD :: Object -> Bool
isPD (PD {}) = True
isPD _ = False
isPT :: Object -> Bool
isPT (PT {}) = True
isPT _ = False
isTCB :: Object -> Bool
isTCB (TCB {}) = True
isTCB _ = False
isPageCap :: Cap -> Bool
isPageCap (PageCap {}) = True
isPageCap _ = False
isPTCap :: Cap -> Bool
isPTCap (PTCap {}) = True
isPTCap _ = False
isPDCap :: Cap -> Bool
isPDCap (PDCap {}) = True
isPDCap _ = False
getChildren :: Object -> Map Integer Cap
getChildren (PD _ children) = children
getChildren (PT _ children) = children
getChildren (TCB _ _ _ _ _ vspace cspace) = fromList $ (0, vspace) : [(1, cspace)]
getChildren _ = error "getChildren called on non-container object"
capCount :: [Object] -> Int
capCount xs = foldl (\acc y -> acc + (length $ elems $ getChildren y)) 0 $ filter isContainer xs
