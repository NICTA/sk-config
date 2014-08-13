--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

module Utils where

import Control.Exception (assert)
import Data.String.Utils (join, endswith, startswith)
import System.FilePath.Posix (splitFileName, dropExtension)

-- |Representation of a targetable architecture.
data Arch =
    Arm
    | IA32
    | Unsupported
    deriving (Eq)
instance Show Arch where
    show Arm = "arm11"
    show IA32 = "ia32"

page_size = 4096 -- bytes

-- Addressable symbol of the stack region (note this does NOT correspond to the
-- stack pointer as there are guard regions either side).
stack_symbol = "_sk_stack"
stack_size = page_size -- bytes

-- More readable mod operations.
(%) = mod

-- Ternary conditional.
True ? x = const x
False ? _ = id

-- Lifted from seL4.
seL4_PageTableBits :: Arch -> Integer
seL4_PageTableBits Arm = 10
seL4_PageTableBits IA32 = 12
pageTableEntrySize = 2 -- bits
seL4_PageBits = 12
pageTableSize :: Arch -> Integer
pageTableSize arch = 2 ^ ((seL4_PageTableBits arch) - pageTableEntrySize + seL4_PageBits)

pageAlignDown :: Integer -> Integer
pageAlignDown x
    | x % page_size == 0 = x
    | otherwise = x - (x % page_size)

pageAlignUp :: Integer -> Integer
pageAlignUp x
    | x % page_size == 0 = x
    | otherwise = x + page_size - (x % page_size)

ptIndex :: Arch -> Integer -> Integer
ptIndex arch vaddr =
    vaddr `div` (pageTableSize arch)
pageIndex :: Arch -> Integer -> Integer
pageIndex arch vaddr =
    (vaddr - (ptVaddr arch vaddr)) `div` page_size

ptVaddr :: Arch -> Integer -> Integer
ptVaddr arch vaddr =
    (ptIndex arch vaddr) * (pageTableSize arch)

indent' :: Int -> [String] -> [String]
indent' level [] = []
indent' level (x:xs) =
    ((join "" $ take ((startswith "}" x) ? (assert (level > 0) (level - 1)) $ level) (repeat "    ")) ++ x):
    (indent' ((endswith "{" x) ? (level + 1) $ ((endswith "}" x) ? (assert (level > 0) level - 1) $ level)) xs)
-- |Indentation functionality for showing source code nicely.
indent :: Int -> String -> String
indent initial_level s = join "\n" $ indent' initial_level (lines s)

-- Get the name of a file, minus path and extension.
fileName :: String -> String
fileName = dropExtension . snd . splitFileName
