--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

-- Functions for generating domain schedule from an input specification.
module OutputConfig (showConfig) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Objects
import Utils

-- |Configuration schedule to be compiled into the kernel. Note that the
-- generated schedule intentionally ignored the initial (booter) thread that
-- shares its domain with the first cell. This can cause some quirks in the
-- boot process if this cell has a low schedule rate.
showConfig :: Double -> Double -> [SpecObject] -> String
showConfig factor total cells =
    unlines ["#include <object/structures.h>",
        "#include <model/statedata.h>",
        "",
        "const dschedule_t ksDomSchedule[] = {",
        schedules,
        "};",
        "",
        "const unsigned int ksDomScheduleLength = sizeof(ksDomSchedule) / sizeof(dschedule_t);"]
    where
        schedules = indent 1 $ unlines $ map (\c ->
            "{ .domain = " ++
            (show $ fromJust $ elemIndex c cells) ++
            ", .length = " ++
            -- NB: total is in µs, factor is Hz and we want time slices.
            (show $ round $ total * (fromIntegral $ cell_rate c) / 100 / (10 ^ 6 / factor)) ++
            "},") cells
