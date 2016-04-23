
-- TODO:
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
{-# LANGUAGE DeriveGeneric #-}

import           Control.Concurrent -- (getNumCapabilities, Chan, writeChan, newChan, forkIO, getChanContents)
import           Control.Monad (when)
import           Control.Parallel.Strategies
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Internal (ByteString(PS))
import           Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
-- Which set is faster?
-- import qualified Data.Set as Set
import qualified Data.HashSet as Set
-- import qualified Data.Vector.Storable.ByteString.Char8 as V
import           Data.Yaml.Char8 as Yaml
import           GHC.Generics
import           System.Environment (getArgs)
import           System.IO (stderr, hPutStrLn)

import Vcd

type Set = Set.HashSet


-- TODO: ++ is slow
headersToList :: [Header] -> [Header]
headersToList (h@(Scope _ hs):hss) = h:headersToList hs ++ headersToList hss
headersToList             (h:hs)   = h:headersToList hs
headersToList              []      = []

aliasesFromSigs :: Set B.ByteString -> [Header] -> Set B.ByteString
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

isTimestamp :: B.ByteString -> Bool
isTimestamp a = "#" `B.isPrefixOf` a

-- BUG: some timestamps aren't getting eaten
eatExtraTimestamps :: [B.ByteString] -> [B.ByteString]
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
 :: Set B.ByteString -- ^ signal ids to keep
 -> B.ByteString -- ^ A chunk
 -> B.ByteString
filterChunk keep bs =
      B.unlines . eatExtraTimestamps . (filter (isKeep keep $!) $!) . mylines $! bs
    where
      mylines = B.lines

filterChunks :: Int -> [B.ByteString] -> Set B.ByteString -> [B.ByteString]
filterChunks nThreads bs keep =
  withStrategy (parBuffer (nThreads * 2) rdeepseq) . map (filterChunk keep) $ bs

{-# INLINE chunk #-}
chunk :: B.ByteString -> [B.ByteString]
chunk bs =
  if B.null bs
    then []
    else
        let (!first, rest) = takeBreakByte chunkSize '\n' bs
        in first : chunk rest

{-# INLINE takeBreakByte #-}
-- combination of take and break so I can do chunks that end at newlines
takeBreakByte :: Int -> Char -> ByteString -> (ByteString, ByteString)
takeBreakByte n c ps@(PS x s l)
    | n <= 0    = ("",ps)
    | n >= l    = (ps,"")
    | otherwise =
       let p = PS x (s + n) (l - n)
       in
         case B.elemIndex c p of
             Nothing -> (ps,"")
             Just m  -> (unsafeTake (n + m) ps, unsafeDrop (n + m) ps)


       -- PS x s n

type Pin = ByteString
type Group = (ByteString,[Pin])

data Config =
  Config
    { pins :: [Pin]
    , groups :: [Group]
    }
    deriving (Show, Generic)

instance FromJSON Config

-- cabal/stack: compile RTS threaded
main :: IO ()
main = do
    [configFile] <- getArgs
    configFileContents <- B.readFile configFile
    let
        config :: Config
        config =
         case decodeEither' configFileContents of
          Right c -> c
          Left e -> error $ configFile ++ ": " ++ show e
        sigsToKeep = Set.fromList (pins config)

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
        chunks = chunk theRest

    let outputs = filterChunks cpus chunks keep
    mapM_ B.putStr outputs

    -- mapM_ B.putStrLn $ eatExtraTimestamps . filter (isKeep keep) . B.lines $ theRest
    -- print $ parseOnly parseSignal theRest
