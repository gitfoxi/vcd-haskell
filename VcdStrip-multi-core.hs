
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Concurrent -- (getNumCapabilities, Chan, writeChan, newChan, forkIO, getChanContents)
import           Control.Monad (when)
import           Control.Parallel.Strategies
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString, unpack)
-- Which set is faster?
import qualified Data.HashSet as Set
import qualified Data.Map as Map
import           Data.Maybe (isNothing, fromJust)
-- import qualified Data.Vector.Storable.ByteString.Char8 as V
import           System.Environment (getArgs)
import           System.IO (stderr, hPutStrLn)

import Config
import Chunk
import Vcd

type Set = Set.HashSet


-- TODO: ++ is slow (but is only called once per scope)
headersToList :: [Header] -> [Header]
headersToList (h@(Scope _ hs):hss) = h:headersToList hs ++ headersToList hss
headersToList             (h:hs)   = h:headersToList hs
headersToList              []      = []

aliasesFromSigs :: Set ByteString -> [Header] -> Set ByteString
aliasesFromSigs sigs =
    Set.fromList .
    map alias .
    filter (flip Set.member sigs . name) .
    filter isWire .
    headersToList
  where
    name  (Wire _ _ nm)  = nm
    name  _              = undefined
    alias  (Wire _ al _) = al
    alias  _             = undefined
    isWire (Wire _ _ _)  = True
    isWire _             = False

{-# INLINE isKeep #-}
isKeep :: Set.HashSet ByteString -> ByteString -> Bool
isKeep !goodsigs !l =
  let (a, sig) = B.splitAt 1 l
      blank = B.null l
  in
      blank
   || a == "#"
   || a == "$"
   || Set.member sig goodsigs


warn :: String -> IO ()
warn s = hPutStrLn stderr $ "Warning: " ++ s

isTimestamp :: ByteString -> Bool
isTimestamp a = "#" `B.isPrefixOf` a

eatExtraTimestamps :: [ByteString] -> [ByteString]
eatExtraTimestamps (a:b:cs) =
  if isTimestamp a && isTimestamp b
    then eatExtraTimestamps (b:cs)
    else a : eatExtraTimestamps (b:cs)
eatExtraTimestamps a = a

chunkSize :: Int
chunkSize = 512000
-- chunkLines = 4000

-- {-# INLINE filterChunk #-}
filterChunk
 :: Set ByteString -- ^ signal ids to keep
 -> ByteString -- ^ A chunk
 -> ByteString
filterChunk keep bs =
      B.unlines . eatExtraTimestamps . (filter (isKeep keep $!) $!) . mylines $! bs
    where
      mylines = B.lines

filterChunks :: Int -> [ByteString] -> Set ByteString -> [ByteString]
filterChunks nThreads bs keep =
  withStrategy (parBuffer (nThreads * 2) rdeepseq) . map (filterChunk keep) $ bs

keepSigs :: Config -> Set Pin
keepSigs conf = Set.fromList . filter (not . forced) . concat .  fmap pins . data'ports $ conf
  where
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
            Just pc -> not . isNothing . force $ pc

-- cabal/stack: compile RTS threaded
main :: IO ()
main = do
    [configFile] <- getArgs
    config <- readConfigFile configFile

    print config -- test

    -- TODO: process ports; don't keep clocks
    let sigsToKeep = keepSigs config

    cpus <- getNumCapabilities
    when (cpus < 48) $
      warn $ "Only " ++ show cpus ++ " cpus detected. We recommend at least 48."

    f <- B.getContents
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

        aliasesToKeep = aliasesFromSigs sigsToKeep
        keep = aliasesToKeep hdrs
        chunks = chunk chunkSize theRest

    let outputs = filterChunks cpus chunks keep
    mapM_ B.putStr outputs

    -- mapM_ B.putStrLn $ eatExtraTimestamps . filter (isKeep keep) . B.lines $ theRest
    -- print $ parseOnly parseSignal theRest
