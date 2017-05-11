{- hub-binl < pattern.hub port label wavetable > pattern.binl

hub = Horizontal Uncompressed Binary

States have been converted to binary wavetable entries

TODO Handle padding -- divisible by 6 for VM, 8 for SM
     aldc subprocess does compression

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Control.Exception
-- import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (isSpace)
-- import           Data.Function ((&))
import           Data.List (nub, sort)
-- import           Data.List.Split (chunksOf)
import           Data.Maybe (fromJust)
-- import           System.Environment (getArgs)
-- import           System.Exit
-- import           System.Process.ByteString
import Options.Applicative
import Data.Semigroup ((<>))

import Util

-- import Debug.Trace

getLength :: ByteString -> Int
getLength s =
  let ls = B.lines s
      ws = B.words (head ls)
  in (fst . fromJust . B.readInt ) (head . drop 1 $ ws)

hubLines :: ByteString -> [(ByteString, ByteString)] -- pin, data
hubLines s =
  let
    (pin, afterPin) = B.break isSpace s
    (num, afterNum) = (fromJust . B.readInt ) (B.dropWhile isSpace afterPin )
    (bin, afterBin) = (B.splitAt num (B.drop 1 afterNum))
  in
    case B.null pin of
      True -> []
      False -> (pin, bin) : hubLines (B.drop 1 afterBin )

data Opts =
  Opts
  { threshold :: Int
  , input :: FilePath -- Input
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> (fmap read . strOption )
      (  long "threshold"
      <> short 't'
      <> value "128"
      <> metavar "THRESHOLD"
      <> help "Minimum number of repeated vectors to trigger repeat insertion")
  <*> argument str
      (  metavar "HUBFILE.hub"
      <> value ""
      <> help "A Horizontal-Uncompressed binary file")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Calculate repeats in horizontal-uncompressed binary and output modified hub with a REPEAT statement at the top"
  <> header "hub-repeat-compress-hub - repeat compression")

enumChanges :: ByteString -> [Int]
enumChanges bs = reverse . go (B.head bs, 1, [0]) $ B.tail bs
  where
    go :: (Char, Int, [Int]) -> ByteString -> [Int]
    go (prevChar, addr, changeList) bs
      | B.null bs = addr : changeList
      | B.head bs == prevChar = go (prevChar, addr + 1, changeList) (B.tail bs)
      | otherwise = go (B.head bs, addr + 1, addr : changeList) (B.tail bs)

mkRepeats :: [(Int,Int)] -> [(Int,Int)]
mkRepeats = go 0
  where
    go _ [] = []
    go acc ((addr, len) : rest) = (addr - acc, len) : go (acc + len - 1) rest

applyRepeats :: [(Int,Int)] -> ByteString -> ByteString
applyRepeats rs bs = B.concat $ go 0 rs bs
  where
    go :: Int -> [(Int,Int)] -> ByteString -> [ ByteString ]
    go _ [] bs = [ bs ]
    go acc ((addr,len):rs) bs =
      let
        a = (addr + 1 - acc)
      in
        B.take a bs : go (acc + a) rs (B.drop (a + len - 1) bs)

format :: (ByteString, ByteString) -> ByteString
format (pin, dat) = B.unwords [pin, B.pack . show $ B.length dat, dat]

unrepeat :: [(Int,Int)] -> ByteString -> ByteString
unrepeat rs bs = B.concat $ go 0 rs bs
  where
    go _ [] bs = [ bs ]
    go acc ((addr,len):rs) bs =
      let (a,b) = B.splitAt (1 + addr - acc) bs
      in
        a : ( B.replicate (len - 1) (B.last a) ) : ( go (1 + addr) rs b )

main :: IO ()
main = do
    Opts threshold inputFile <- execParser opts
    contents <- getInput inputFile

    let
      length = getLength contents
      hs = hubLines contents
      changeCycles = nub . sort . concatMap ( enumChanges . snd ) $ hs
      diffs = diff changeCycles
      candidates = filter ( (> threshold) . snd ) (zip changeCycles diffs)
      repeats = mkRepeats candidates
      datOut = map (applyRepeats repeats . snd) hs
      unrepeatTest = map ( unrepeat repeats ) datOut
      matches = map snd hs == unrepeatTest

    -- print changeCycles
    -- print diffs
    -- print $ filter ( (> threshold) . snd ) (zip changeCycles diffs)
    putStrLn $ "REPEATS " ++ show repeats
    -- mapM_ ( B.putStrLn . vecd paddedLength ) (hubLines paddedDat)

    mapM_ (B.putStrLn . format) (zip (map fst hs) datOut)
    -- print $ "Matches: " ++ matches show
    
