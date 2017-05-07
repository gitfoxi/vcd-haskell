{- vcd-transpose-hcd < pattern.vcd

-- Clocks in signals on rising jtg_tck

-- TODO clock outputs on falling tck
-- TODO change timestamp to 200ns

This code doesn't stream because it has to read the last line of input before it
can write the first line of output.

Memory and runtime will suffer with large inputs.

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow (second)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.List (foldl')
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )
import           Data.Maybe (fromJust)

import Util
import Vcd

mkAliasWireMap :: [Header] -> HashMap ByteString ByteString
mkAliasWireMap hdrs =
  let flat = flattenHeaders hdrs
      wires = filter isWire flat
      aliasWire (Wire _ al nm) = (al, nm)
      aliasWire x = error $ "aliasWire " ++ show x
  in
    HashMap.fromList (map aliasWire wires)

splitByTime :: [ByteString] -> [ (Int, [ ( Char, ByteString ) ]) ]
splitByTime = splitByTime' []
  where
    splitByTime' ::
      [(Int, [ (Char, ByteString) ])]
      -> [ByteString]
      -> [(Int, [ (Char, ByteString) ])]
    splitByTime' acc (l:ls)
      | isTimestamp l =
        splitByTime'
        ( ( fst . fromJust . B.readInt . B.tail $ l, [] ):acc )
        ls
      | otherwise =
        splitByTime' (prepend ( fromJust . B.uncons $ l) acc) ls
    splitByTime' acc [] = reverse acc
    prepend a ((tm, as) : rest ) =
      (tm, a:as) : rest
    prepend _ [] = error "Prepend called on empty list"

main :: IO ()
main = do

    f <- B.getContents

    let
        (hdrs, theRest) = splitHeaders f
        aliasWireMap = mkAliasWireMap hdrs
        timeStates = splitByTime . B.lines $ theRest
        start :: HashMap ByteString [(Int, Char)]
        start = HashMap.fromList (zip (HashMap.keys aliasWireMap) (repeat []))
        -- updates = map mkUpdate timeStates
        x =
          map (second reverse) .
          HashMap.toList .
          foldl' updat start $
          timeStates
        updat ::
          HashMap ByteString [(Int, Char)]
          -> (Int, [ ( Char, ByteString ) ])
          -> HashMap ByteString [(Int, Char)]
        updat acc (tm, changes) = foldl' (updat' tm) acc changes
        updat' ::
          Int
          -> HashMap ByteString [(Int, Char)]
          -> (Char, ByteString)
          -> HashMap ByteString [(Int, Char)]
        updat' tm acc (state, alias) = HashMap.insertWith (++) alias [ (tm, state ) ] acc

        maxTime :: Int
        maxTime = fst . last $ timeStates

        diffTimes :: [Int] -> [Int]
        diffTimes (i:is) = zipWith (-) (is ++ [ maxTime ]) (i:is)
        diffTimes [] = []

        format (al, cs) =
          B.unlines
          [ fromJust . HashMap.lookup al $ aliasWireMap
          , B.unwords (map (B.pack . show) (diffTimes  (map fst cs)))
          , B.pack (map snd cs)
          ]


    mapM_ (B.putStr . format) x
