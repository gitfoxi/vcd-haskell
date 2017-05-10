{- hus-custom-wavetable X < pattern.hus

   X is a number from 1 to 8 specifying the X mode

   Gene an expanded, transposed (horizontal, if you will) file like

    PINA XXHHHHLLL
    PINB 01101Z010

To binary bytes as given by the wavetable

TODO This is a bit slower than I'd like
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (chr)
import           Data.List (sort, nub)
import           Data.Hashable
import qualified Data.HashSet as Set
import           System.Environment (getArgs)

import Util

data WaveTable
  = WaveTable
  { pins :: [ByteString]
  , waves :: [ByteString]
  } deriving Show

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bs
  | B.null bs = []
  | otherwise = B.take n bs : chunksOf n (B.drop n bs)

unique :: Hashable a => Eq a => [a] -> [a]
unique = Set.toList . Set.fromList

mkTable :: Int -> ByteString -> WaveTable
mkTable xmode inp =
  let
    [pin, statesIn] = B.words inp
    states = pad xmode . canonical $ statesIn
    table = sort . unique . nub . chunksOf xmode $ states
  in
    WaveTable [pin] table

main :: IO ()
main = do
  [xmode] <- map read <$> getArgs :: IO [Int]
  contents <- B.getContents

  let
    rawTables = map (mkTable xmode) (B.lines contents)

  print rawTables
