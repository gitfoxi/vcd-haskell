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

data Opts =
  Opts
  { optForceFile :: FilePath
  , optInput :: FilePath
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> strOption
      (  long "force"
      <> short 'f'
      <> metavar "FORCE_FILE.txt"
      <> help "File containing one space-separated pair of WIRE STATE per line")
  <*> argument str
      (  metavar "HCDFILE.hcd"
      <> value ""
      <> help  "A Horizontal-Compressed Dump file")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Add, mask or force wires in an HCD whether a wire with that name already existed or not. Example line in the FORCE_FILE.txt: 'stupid_sig_n x'"
  <> header "hcd-force - force static wires")


main :: IO ()
main = do
    Opts forceFile vcdFile <- execParser opts
    ff <- B.readFile forceFile
    f <- getInput vcdFile

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

