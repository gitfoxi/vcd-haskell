
-- TODO:
--  mmap-bytestring to skip initial slow loading
--  Decompress: .gz .bz2 .xz
--  Compress: .bz2 using pbzip2; fallback to .gz
--  Warn if they don't use .bz2 for input or output
--  Warn if < 48 cores
--  Pins you want to keep (instead of just pins you want to toss)
--  Also filter multiple consecutive timestamp lines, keeping only the last
--
--   #100
--   #200
--
--   becomes
--
--   #200
--
--  YAML config
--
-- Try memoizing each chunk; a smaller set of already-seen signals can be discarded faster maybe (tried; no help)
--
-- TODO: Try benchmarking on a computer with many cores. Only getting 2x speedup on my 4-core computer


module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as Set

import Lib

type Set = Set.HashSet


-- TODO: ++ is slow (but is only called once per scope)
-- Make Header Foldable and use Data.Foldable.toList or use a builder
-- headersToList headers
-- flattens a list of headers with nested scopes into a single list
-- So clearly, we don't support scoped signals
headersToList :: [Header] -> [Header]
headersToList (h@(Scope _ hs):hss) = h:headersToList hs ++ headersToList hss
headersToList             (h:hs)   = h:headersToList hs
headersToList              []      = []

-- aliasesFromSigs sigNameSet headers
-- Takes a list of signals and uses the headers to return a the set of aliases for those signals
-- Only supports Wire signals, not busses or anything else.
aliasesFromSigs :: Set ByteString -> [Header] -> Set ByteString
aliasesFromSigs sigs =
    Set.fromList .
    map alias .
    filter (flip Set.member sigs . name) .
    filter isWire .
    headersToList

-- name header
-- returns the name of the wire
-- fails if called on any Header other than a Wire
-- TODO: there must be a better way
name :: Header -> ByteString
name  (Wire _ _ nm)  = nm
name  _              = undefined

-- alias header
-- returns the alias of the wire
-- fails if called on any Header other than a Wire
-- TODO: there must be a better way
alias :: Header -> ByteString
alias  (Wire _ al _) = al
alias  _             = undefined

-- isKeep goodSigs line
-- True iff line is something we want to keep
-- - Timestamp
-- - State change for a signal in goodSigs
{-# INLINE isKeep #-}
isKeep :: Set.HashSet ByteString -> ByteString -> Bool
isKeep !goodsigs !l =
  let (a, sig) = B.splitAt 1 l
  in
   a == "#"
   || Set.member sig goodsigs 

-- eatExtraTimestamps lines
--
-- Filters out any extra timestamps remaining in lines, defined as two or more timestamps with nothing in between. Keep the last in a list of timestamps
eatExtraTimestamps :: [ByteString] -> [ByteString]
eatExtraTimestamps ls = map last (groupBy bothAreTimestamps ls)
  where
    bothAreTimestamps a b = isTimestamp a && isTimestamp b
-- eatExtraTimestamps (a:b:cs) =
--   if isTimestamp a
--     then if isTimestamp b
--       then eatExtraTimestamps (b:cs)
--       else a : b : eatExtraTimestamps cs
--     else a : eatExtraTimestamps (b:cs)
-- eatExtraTimestamps a = a

-- chunkSize
-- Tune this parameter for performance
-- Bigger is faster but takes more memory
chunkSize :: Int
chunkSize = 262144 `div` 2



-- filterChunk keepSigs Chunk
--
-- filters the signal lines in one chunk to discard massive amounts of crap
-- {-# INLINE filterChunk #-}
filterChunk
 :: Set.HashSet ByteString -- ^ signal ids to keep
 -> ByteString -- ^ A chunk
 -> [ ByteString ]
filterChunk keep bs =
    (eatExtraTimestamps . filter (isKeep keep $!) $!) . mylines $! bs
    where
      mylines = B.lines

filterChunks :: Int -> [ByteString] -> Set ByteString -> [ ByteString ]
filterChunks nThreads bs keep =
      -- map (filterChunk keep) $ bs  -- single-threaded debug
      -- map B.unlines . map eatExtraTimestamps
  (eatExtraTimestamps . concat ) ( withStrategy (parBuffer ( nThreads * 20 ) rdeepseq) . map (filterChunk keep) $ bs)

data Opts =
  Opts
  { optKeepFile :: FilePath
  , optInput :: FilePath
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> strOption
      (  long "keepWires"
      <> short 'k'
      <> metavar "FILENAME.txt"
      <> help "File containing whitespace-separated list of pins to keep")
  <*> argument str
      (  metavar "VCDFILE.vcd"
      <> value ""
      <> help "A Vector-Change Dump file")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Remove unwanted wires and other noise so subsequent tools can be fast and don't have to worry about obscure constructs. For best results, remove all free-running clocks and add them back as break vectors in their own port. Any conversion tool will run much faster with vcd-strip in front of it."
  <> header "vcd-strip - minimize VCD before further conversion")

-- cabal/stack: compile RTS threaded
main :: IO ()
main = do
    traceEventIO "hello world"
    Opts configFile vcdFile <- execParser opts
    -- f <- B.readFile vcdFile
    -- f <- mmapFileByteString vcdFile Nothing
    f <- getInput vcdFile -- TODO: getInput to mmapFileByteString if its a regular file
    traceEventIO "mmap done"
    performGC
    traceEventIO "GC done"
    config <- B.readFile configFile
    traceEventIO "config reading done"

    -- print config -- test

    -- TODO: process ports; don't keep clocks
    let sigsToKeep = Set.fromList . B.words $ config
    traceEventIO "config parsing done"

    cpus <- getNumCapabilities
    when (cpus < 48) $
      traceEventIO $ "Only " ++ show cpus ++ " cpus detected. We recommend at least 48."

    -- TODO: probably a big pause while we read the whole file
    -- If its a real file, mmap it
    -- If its a stream, read chunks
    -- Read a stream
    -- f <- B.getContents
    let res = parse parseAllHeaders f
        (hdrs, theRest) =
          case res of
            Fail unconsumed contexts message ->
                error $ "VCD header parse failed: "
                     ++ message
                     ++ "\n---- Contexts ----\n"
                     ++ unlines contexts
                     ++ "\n------------------\n"
                     ++ "\nUnconsumed:\n" ++ Prelude.take 200 (B.unpack unconsumed)
                     ++ "\n...\n"
            Partial _ ->
                error $ "VCD header parse failed: "
                     ++ "EOF during header"
            Done theRest' hdrs' -> (hdrs', theRest')

        hdrsOut =
          filterHeaders
            (\h -> (not . isWire) h || Set.member (name h) sigsToKeep)
            hdrs

        aliasesToKeep = aliasesFromSigs sigsToKeep
        keep = aliasesToKeep hdrs
        chunks = chunk chunkSize theRest
    performGC

    let outputs = filterChunks cpus chunks keep

    hPutBuilder stdout (foldMap render hdrsOut)
    -- XXX Ugly hack, should add a top-level header to contain the others
    B.hPutStrLn stdout "$enddefinitions $end"
    mapM_ B.putStrLn outputs

    -- mapM_ B.putStrLn $ eatExtraTimestamps . filter (isKeep keep) . B.lines $ theRest
    -- print $ parseOnly parseSignal theRest
