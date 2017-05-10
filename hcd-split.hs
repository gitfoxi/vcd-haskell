{- hcd-split portA.hcd portApins.txt portB.hcd portBpins.txt ...

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Function ((&))
import qualified Data.HashSet as Set
import           Data.List.Split (chunksOf)
import           System.Environment (getArgs)

import qualified Hcd
import           Hcd (Hcd)

mkPairs :: [String] -> [(String, String)]
mkPairs ( a:b:rest ) = (a,b) : mkPairs rest
mkPairs _ = []

writePort :: [Hcd] -> (FilePath, [ByteString] ) -> IO ()
writePort hcds ( file, pins ) =
  let
    pinSet = Set.fromList pins
    portHcds = filter (flip Set.member pinSet . Hcd.pin) hcds
  in
    B.writeFile file (B.concat (map Hcd.toByteString
                               portHcds))

main :: IO ()
main = do
    -- portA portApins.txt portB portBpins.txt ...
    args <- getArgs

    f <- B.getContents

    let
      (ports, pinFiles) = unzip $ mkPairs args
    pinContents <- mapM B.readFile pinFiles

    let
      pinLists = map B.words pinContents

      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map Hcd.fromList

    mapM_ (writePort hcds) (zip ports pinLists)
