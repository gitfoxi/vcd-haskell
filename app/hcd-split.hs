{- hcd-split portA.hcd portApins.txt portB.hcd portBpins.txt ... -i all.hcd

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as Set

import Lib

mkPairs :: [String] -> [(String, String)]
mkPairs ( a:b:rest ) = (a,b) : mkPairs rest
mkPairs _ = []

writePort :: [Hcd] -> (FilePath, [ByteString] ) -> IO ()
writePort hcds ( file, pins ) =
  let
    pinSet = Set.fromList pins
    portHcds = filter (flip Set.member pinSet . hcdPin) hcds
  in
    B.writeFile file (B.concat (map toByteString
                               portHcds))

data Opts =
  Opts
  { optInput :: FilePath
  , optOutHcdOrPinList :: [String]
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> strOption
      (  long "input"
      <> short 'i'
      <> metavar "INPUT_FILE.hcd"
      <> help "The input file to split (or use stdin)")
  <*> some (argument str
      (  metavar "A.hcd Apin.txt ..."
      <> help  "Pairs of output HCD files and text files containing the space-separated list of corresponding pins"))

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Split an HCD into several for multiport. Also useful to delete pins you don't want in your final output (like io control pins)."
  <> header "hcd-split - split one HCD into one or more HCDs")


main :: IO ()
main = do
    -- portA portApins.txt portB portBpins.txt ...
    Opts inpFile hcdPinPairs <- execParser opts
    f <- getInput inpFile

    let
      (ports, pinFiles) = unzip $ mkPairs hcdPinPairs
    pinContents <- mapM B.readFile pinFiles

    let
      pinLists = map B.words pinContents

      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map fromList

    mapM_ (writePort hcds) (zip ports pinLists)
