{- hcd-expand-hus < pattern.hcd > pattern.hus

hus = Horizontal Uncompressed States

-- Clocks in signals on rising jtg_tck

-- TODO clock outputs on falling tck
-- TODO change timestamp to 200ns

This code doesn't stream because it has to read the last line of input before it
can write the first line of output.

Memory and runtime will suffer with large inputs.

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Function ((&))
import           Data.List.Split (chunksOf)
import           Data.Maybe (fromJust)

main :: IO ()
main = do

    f <- B.getContents

    let
      hus :: [ByteString]
      hus =
        B.lines f &
        chunksOf 3 &
        map expand

      expand :: [ByteString] -> ByteString
      expand [ pin,lens,states ] =
        B.unlines [ pin
                  , B.concat (zipWith
                              (\len state -> B.replicate (fst . fromJust $ B.readInt len ) state) (B.words lens ) (B.unpack states ))]
      expand x = error $
        "expand called without 3 lines"
        ++ show x

    mapM_ B.putStr hus
