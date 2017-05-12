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
import qualified Data.HashSet as Set

import Lib

ls2Hcd [p,l,s] = Hcd p l s

hcdToBS (Hcd p l s) = B.unlines [p, l, s]

main :: IO ()
main = do
    [forceFile] <- getArgs
    ff <- B.readFile forceFile

    f <- B.getContents

    let
      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map ls2Hcd

      forces =
        B.lines ff &
        map B.words &
        map (\[p,s] -> (p,s))

      forceSet = Set.fromList (map fst forces)

    mapM_ (B.putStr . hcdToBS)
      (filter (not . flip Set.member forceSet . hcdPin) hcds)

    mapM_ (B.putStr . hcdToBS .  (\(p,s) -> Hcd p "1" s)) forces

