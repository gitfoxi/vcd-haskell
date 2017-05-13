{- v-rename rename.txt < pattern.vcd

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HashMap

import Lib


mapNames ::
  HashMap ByteString ByteString
  -> Header
  -> Header
mapNames nameMap (Wire n al nm) = Wire n al (HashMap.lookupDefault nm nm nameMap)
mapNames _ a = a


data Opts =
  Opts
  { optRenameFile :: FilePath
  , optInput :: FilePath
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> strOption
      (  long "renameFile"
      <> short 'r'
      <> value ""
      <> metavar "FILENAME.txt"
      <> help "File containing one space-separated pair of OLDNAME NEWNAME per line")
  <*> argument str
      (  metavar "VCDFILE.vcd"
      <> value ""
      <> help  "A Vector-Change Dump file")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Rename wires in a VCD"
  <> header "vcd-rename - Rename wires")

main :: IO ()
main = do
    Opts renameFile vcdFile <- execParser opts
    renameContents <- B.readFile renameFile
    f <- getInput vcdFile

    let
      -- XXX Will not handle bad input like a blank line
      toTuple [a, b] = (a, b)
      nameMap =
        HashMap.fromList (map ( toTuple . B.words ) ( B.lines renameContents ))


    let
        (hdrs, theRest) = splitHeaders f

        hdrsOut =
          mapHeaders (mapNames nameMap) hdrs

    hPutBuilder stdout (foldMap render hdrsOut)
    -- XXX Ugly hack, should add a top-level header to contain the others
    B.putStrLn "$enddefinitions $end"
    -- TODO: probably won't stream because has to read the whole input before beginning output
    B.putStr theRest
