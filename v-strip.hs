
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Concurrent -- (getNumCapabilities, Chan, writeChan, newChan, forkIO, getChanContents)
import           Control.Monad (when)
import           Control.Parallel.Strategies
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Builder (hPutBuilder)
-- Which set is faster? ANSWER: hashset
-- DONE: bloom filter, perfect hash suck; bytestring-trie disclaims it is worse that HashSet; bitset is same
import qualified Data.HashSet as Set
import qualified Data.Map as Map
import           Data.Maybe (isJust)
-- import qualified Data.Vector.Storable.ByteString.Char8 as V
import           System.Environment (getArgs)
import           System.IO (stderr, hPutStrLn, stdout)

import Config
import Chunk
import Vcd

import Data.List (groupBy)
import Debug.Trace
import System.IO.MMap

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

-- isWire header
-- True iff header is a Wire
isWire :: Header -> Bool
isWire Wire{} = True
isWire _             = False

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


-- warn message
--
-- Print a warning message on stderr
warn :: String -> IO ()
warn s = hPutStrLn stderr $ "Warning: " ++ s

-- isTimestamp line
--
-- True iff line represents a timestamp
-- {-# INLINE isTimestamp #-}
isTimestamp :: ByteString -> Bool
isTimestamp a = "#" `B.isPrefixOf` a

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
chunkSize = 262144



-- filterChunk keepSigs Chunk
--
-- filters the signal lines in one chunk to discard massive amounts of crap
-- {-# INLINE filterChunk #-}
filterChunk
 :: Set.HashSet ByteString -- ^ signal ids to keep
 -> ByteString -- ^ A chunk
 -> [ ByteString ]
filterChunk keep bs =
      (filter (isKeep keep $!) $!) . mylines $! bs
    where
      mylines = B.lines

filterChunks :: Int -> [ByteString] -> Set ByteString -> ByteString
filterChunks nThreads bs keep =
      -- map (filterChunk keep) $ bs  -- single-threaded debug
      -- map B.unlines . map eatExtraTimestamps
      (B.unlines . eatExtraTimestamps . concat ) ( withStrategy (parBuffer ( nThreads * 20 ) rdeepseq) . map (filterChunk keep) $ bs)

keepSigs :: Config -> Set Pin
keepSigs conf = Set.fromList (data'port'pins ++ jtag'pins)
  where
    data'port'pins = filter (not . forced) . concatMap pins . data'ports $ conf
    jtag'pins =
      let  jt = jtag'port conf
      in mux jt ++ map (\f -> f jt) [tdi, tck, tdo, tms]
    forced :: Pin -> Bool
    -- TODO: Error if pin not found in all'pins
    --       Lens
    forced k =
      let mmpc = Map.lookup k (all'pins conf)
      in case mmpc of
        Nothing -> error $ "Missing " ++ B.unpack k ++ " in config"
        Just mpc ->
          case mpc of
            Nothing -> False
            Just pc -> isJust . force $ pc


-- cabal/stack: compile RTS threaded
main :: IO ()
main = do
    [configFile, vcdFile] <- getArgs
    config <- readConfigFile configFile

    -- print config -- test

    -- TODO: process ports; don't keep clocks
    let sigsToKeep = keepSigs config

    cpus <- getNumCapabilities
    when (cpus < 48) $
      warn $ "Only " ++ show cpus ++ " cpus detected. We recommend at least 48."

    -- TODO: probably a big pause while we read the whole file
    -- If its a real file, mmap it
    -- If its a stream, read chunks
    -- Read a stream
    -- f <- B.getContents
    f <- mmapFileByteString vcdFile Nothing
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

    let outputs = filterChunks cpus chunks keep

    hPutBuilder stdout (foldMap render hdrsOut)
    B.putStr outputs

    -- mapM_ B.putStrLn $ eatExtraTimestamps . filter (isKeep keep) . B.lines $ theRest
    -- print $ parseOnly parseSignal theRest
