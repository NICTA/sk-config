--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

import Control.Exception (assert)
import Control.Monad (when)
import Data.Elf (Elf)
import Data.String.Utils (join, replace) -- MissingH
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.List as L (nub, sort)

import qualified CapDLObjects as CapDL
import Objects
import OutputC
import OutputCapDL
import OutputConfig
import XMLWrapper
import ELFFile
import Utils

-- |Type of command line options.
data Options
    = Architecture { opt_arch :: Arch }
    | Debug
    | Help
    | InputELF { opt_filename :: String }
    | InputXML { opt_filename :: String }
    | Output { opt_output :: String }
    | SelectedCell { opt_cell :: String }
    | Tick { opt_tick :: Integer }
    deriving (Eq)

options :: [OptDescr Options]
options =
    [ Option []    ["arm"]    (NoArg (Architecture Arm))             "assume an ARM platform"
    , Option []    ["ia32"]   (NoArg (Architecture IA32))            "assume an IA32 platform"
    , Option ['?'] ["help"]   (NoArg Help)                           "display help options"
    , Option ['c'] ["cell"]   (ReqArg (\a -> SelectedCell a) "CELL") "operate on cell CELL"
    , Option ['d'] ["debug"]  (NoArg Debug)                          "enable debugging output"
    , Option ['e'] ["elf"]    (ReqArg (\a -> InputELF a) "FILE")     "read FILE as the compiled application"
    , Option ['o'] ["output"] (ReqArg (\a -> Output a) "ELEMENT")    "write generated artefact ELEMENT (\"capdl\", \"config\", \"header\" or \"source\")"
    , Option ['t'] ["tick"]   (ReqArg (\a -> Tick $ read a) "FREQUENCY") "Time slices per second (only relevant for scheduler configuration)"
    , Option ['x'] ["xml"]    (ReqArg (\a -> InputXML a) "FILE")     "read FILE as the input specification"
    ]

-- |Prints command line usage information.
usage :: String
usage = usageInfo ("Usage: " ++ (unsafePerformIO getProgName) ++ " options\n") options

-- |Prints debugging output.
debugPutStrLn :: Bool -> String -> IO()
debugPutStrLn enabled message =
    when enabled $ hPutStrLn stderr message

-- |Parse command line arguments.
parseArgs :: IO [Options]
parseArgs = do
    args <- getArgs
    let (opts, nonopts, errs) = getOpt RequireOrder options args
    return $ if (not $ null nonopts)
        then error ("Unrecognised arguments: " ++ (join ", " nonopts) ++ "\n" ++ usage)
        else if (not $ null errs)
            then error ("Failed to parse arguments: " ++ (head errs) ++ "\n" ++ usage)
            else if (Help `elem` opts)
                then error usage
                else opts

-- |Write C source.
writeC :: SpecObject -> [SpecObject] -> [SpecObject] -> Bool -> IO()
writeC cell channels segments debug =
    assert (all isSegment segments) $ do
        debugPutStrLn debug ("Writing " ++ (cell_name cell) ++ "_driver.c")
        writeFile ((cell_name cell) ++ "_driver.c") (showSource (cell_name cell) (cell_segs cell) segments channels)

-- |Write C header.
writeH :: SpecObject -> [SpecObject] -> Bool -> IO()
writeH cell channels debug = do
    debugPutStrLn debug ("Writing " ++ (cell_name cell) ++ "_driver.h")
    writeFile ((cell_name cell) ++ "_driver.h") (showHeader (cell_name cell) (cell_segs cell) channels)

-- |Write CapDL spec.
writeCapDL :: Arch -> Options -> M.Map String (IO Elf) -> [SpecObject] -> [SpecObject] -> Bool -> IO()
writeCapDL arch spec elfs cells segments debug =
    assert (all isSegment segments) $ do
    mapM_ (\x -> do
        elf <- elfs M.! x
        debugPutStrLn debug ("ELF file " ++ x ++ " has " ++ (show $ length $ getFrames elf) ++ " frames.")) (M.keys elfs)

    objs <- mapM (\x ->
            let name = cell_name x
                use_segments = cell_segs x
                domain = fromJust (elemIndex x cells)
            in do
                elf <- elfs M.! name
                return $ toCapDL arch (name, elf, domain) segments use_segments $ getFrames elf) cells

    debugPutStrLn debug ((show $ length objs) ++ " collections of caps and objects derived.")
    mapM_ (\x -> debugPutStrLn debug (" (" ++ (show $ length x) ++ " objects, " ++ (show $ CapDL.capCount x) ++ " caps)")) objs

    let capdlSpec = toCapDLSpec arch objs

    debugPutStrLn debug ("Collapsed into " ++ (show $ length $ CapDL.spec_objs capdlSpec) ++ " objects and " ++ (show $ CapDL.capCount $ CapDL.spec_objs capdlSpec) ++ " caps.")
    debugPutStrLn debug ("Writing " ++ (fileName $ opt_filename spec) ++ ".cdl")

    writeFile ((fileName $ opt_filename spec) ++ ".cdl") (show capdlSpec)

writeConfig tick layout cells =
    writeFile "config.c" (showConfig (fromIntegral tick) (fromIntegral runtime) cells)
    where
        runtime = read $ getJustAttr layout "runtime"

makeChannelSegments :: SpecObject -> [SpecObject]
makeChannelSegments c@(Channel _ _ _ msgsize slots _) =
    [buffer_segment, reader_segment, writer_segment]
    where
        buffer_size = msgsize * slots
        buffer_segment = Segment (channelBufferSegmentName c) buffer_size Nothing
        reader_segment = Segment (channelReaderSegmentName c) 4 Nothing
        writer_segment = Segment (channelWriterSegmentName c) 4 Nothing

addChannelUseSegments :: [SpecObject] -> SpecObject -> SpecObject
addChannelUseSegments channels (Cell name rate usesegs) =
    Cell name rate (usesegs ++ (segments True) ++ (segments False))
    where
        channels' :: Bool -> [SpecObject]
        channels' True = fromChannels name channels
        channels' False = toChannels name channels
        channelSeg name read write = UseSegment name name read write
        bufSeg from channel = channelSeg (channelBufferSegmentName channel) (not from) from
        writeSeg from channel = channelSeg (channelWriterSegmentName channel) (not from) from
        readerSeg from channel = channelSeg (channelReaderSegmentName channel) from (not from)
        allSegs :: Bool -> [SpecObject -> SpecObject]
        allSegs from = [bufSeg from, writeSeg from, readerSeg from]
        segments from = concatMap (\c -> map (\s -> s c) (allSegs from)) (channels' from)

main = do

    -- Parse command line arguments.
    opts <- parseArgs

    let debug = Debug `elem` opts
    debugPutStrLn debug "Debugging enabled"

    -- Determine which architecture we're targeting (default ARM).
    let arches = filter (\x -> case x of
            (Architecture _) -> True
            _ -> False) opts
    let arch = case arches of
            [] -> Arm
            [Architecture a] -> a
            _ -> error "multiple architecture options provided"
    when (arch == Unsupported) $
        error "Unsupported architecture specified"
    debugPutStrLn debug ("Architecture: " ++ (show arch))

    -- Parse the input specification.
    let inputSpecs = filter (\x -> case x of
            (InputXML _) -> True
            _ -> False) opts
    let inputSpec = case inputSpecs of
            [] -> error "no XML input specification provided"
            [InputXML x] -> InputXML x
            _ -> error "multiple XML input specifications provided"
    debugPutStrLn debug ("Input spec: " ++ (opt_filename inputSpec))
    xml <- readXML $ opt_filename inputSpec

    let layout = getJustElement (getRoot xml) "layout"
    let channels = map parseChannel (getElements layout "channel")
    debugPutStrLn debug ((show $ length channels) ++ " channels parsed.")
    let xmlsegments = map parseSegment (getElements layout "segment")
    debugPutStrLn debug ((show $ length xmlsegments) ++ " segments parsed.")
    let xmlcells = map parseCell (getElements layout "cell")
    debugPutStrLn debug ((show $ length xmlcells) ++ " cells parsed.")
    -- Add the implicit channel segments to the explicit segments in the xml
    let segments = xmlsegments ++ concatMap makeChannelSegments channels
    debugPutStrLn debug ((show $ length segments) ++ " actual segments.")
    -- Add the implicit channel connections to the cells
    let cells = map (addChannelUseSegments channels) xmlcells

    -- Parse ELF input files into a (Map <cellname>::String <file>::Elf).
    let elfs = M.fromList $ map ((\x -> (fileName x, openElf x)) . opt_filename) $ filter (\x -> case x of
            (InputELF _) -> True
            _ -> False) opts
    debugPutStrLn debug ((show $ length $ M.keys elfs) ++ " ELFs opened.")

    -- Some sanity checks.

    -- Each use-segment in a cell should refer to a segment defined elsewhere
    -- in the layout.
    mapM (\c ->
        mapM (\u -> if (useseg_name u `notElem` (map seg_name segments))
                then error $ cell_name c ++ " references segment " ++ useseg_name u ++ " that does not exist in the input specification"
                else return ())
            (cell_segs c))
        cells

    -- Check for cell name collisions.
    when ((\x -> L.nub x /= x) $ map cell_name cells) $
        error "duplicate cell names defined"

    -- Check for segment name collisions.
    when ((\x -> L.nub x /= x) $ map seg_name segments) $
        error "duplicate segment names defined"

    -- The ELF files we were passed should match the cells from the XML spec
    -- 1-1 if we were passed any ELFs.
    assert ((M.null elfs) ||
        ((L.sort $ map cell_name cells) == (L.sort $ M.keys elfs)))
        (return ())

    -- FIXME: It would be nice to assert that any ELF files we were passed are
    -- the same architecture as the one we are targeting, but this is a bit
    -- monadically tricky.
    -- when (any (\x -> elfArch x /= arch) (M.elems elfs)) $
    --     error "The format of one or more ELFs does not match your target architecture"

    -- Check for 0-size segments. This won't cause a generator error, but will
    -- produce arrays with an invalid length.
    mapM (\s -> if (seg_size s == 0)
            then debugPutStrLn debug ("Warning: segment " ++ seg_name s ++ " has size 0")
            else return ())
        segments

    -- Check for shared memory regions with no permissions. This won't cause a
    -- generator error, but will produce shared memory regions that the cell
    -- can't use.
    mapM (\c ->
        mapM (\u -> if (not (useseg_read u || useseg_write u))
                then debugPutStrLn debug ("Warning: cell " ++ cell_name c ++ " has an unusable shared memory region " ++ useseg_alias u)
                else return ())
            (cell_segs c))
        cells

    -- Check for shared memory regions with the same alias in a single cell.
    -- This won't cause a generator error, but will produce symbol collisions
    -- in C.
    mapM (\c ->
        if ((length $ L.nub $ map useseg_alias $ cell_segs c) /= (length $ map useseg_alias $ cell_segs c))
            then debugPutStrLn debug ("Warning: cell " ++ cell_name c ++ " has two shared memory regions with the same symbol")
            else return ())
        cells

    -- Are we producing CapDL, C source or a C header?
    let output = case filter (\x -> case x of Output _ -> True; _ -> False) opts of
            [] -> error "no -o/--output option given"
            [Output x] -> x
            _ -> error "multiple -o/--output options given"

    -- Now write the output file.
    if output == "capdl" then
        if M.null elfs then
            error "no ELF files provided"
        else
            writeCapDL arch inputSpec elfs cells segments debug
    else if output == "config" then do
        let tick = case filter (\x -> case x of Tick _ -> True; _ -> False) opts of
                [] -> error "no -t/--tick option given"
                [Tick t] -> t
                _ -> error "multiple -t/--tick options given"
        writeConfig tick layout cells
    else do
        -- We need to consider what cell the user requested.
        let name = case filter (\x -> case x of SelectedCell _ -> True; _ -> False) opts of
                [] -> error "no -c/--cell option given"
                [SelectedCell x] -> x
                _ -> error "multiple -c/--cell options given"
        let cell = case filter (\x -> name == cell_name x) cells of
                [] -> error ("no cell \"" ++ name ++ "\" found")
                [x] -> x
                _ -> error ("multiple definitions of cell \"" ++ name ++ "\" found")
        if output == "source" then
            writeC cell channels segments debug
        else if output == "header" then
            writeH cell channels debug
        else
            error ("invalid --output option \"" ++ output ++ "\" specified")
