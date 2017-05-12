{- hcd-static < pattern.hcd > static.hcd

Convert and hcd that only contains static pins to a 1-cycle hcd to save memory
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as Set

import Lib

toOuput :: Hcd -> Hcd
toOuput (Hcd p l s) =
  Hcd p l (B.map outMap s)
  where
    outMap '0' = 'l'
    outMap '1' = 'h'
    outMap _ = 'x'

main :: IO ()
main = do
    [outputsFile] <- getArgs
    outputsContent <- B.readFile outputsFile
    f <- B.getContents

    let
      outputsSet = Set.fromList . B.words $ outputsContent

      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map fromList

      hcdsOut =
        map (\h ->
               if Set.member (hcdPin h) outputsSet
               then toOuput h
               else h) hcds

    mapM_ ( B.putStr . toByteString ) hcdsOut

