{- hcd-split-static
TODO Also output dynamic list
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B

import Lib

ls2Hcd [p,l,s] = Hcd p l s

isStatic :: Hcd -> Bool
isStatic (Hcd _ l _) =
  let ws = B.words l
  in
    (not . null) ws && (null . tail) ws

mkStaticOut :: [Hcd] -> ByteString
mkStaticOut hcds =
  map mkOne hcds
  & B.concat
  where
    mkOne (Hcd p _ s) =
      B.unlines [p, "1", head . B.words $ s]

mkDynamicOut :: [Hcd] -> ByteString
mkDynamicOut hcds =
  map mkOne hcds
  & B.concat
  where
    mkOne (Hcd p l s) = B.unlines [p, l, s]

mkPinList :: [Hcd] -> ByteString
mkPinList hcds =
  map hcdPin hcds
  & sort
  & B.unlines

data Opts =
  Opts
  { optStaticHcd :: String
  , optDynamicHcd :: String
  , optStaticList :: String
  , optDynamicList :: String
  , optVcdFile :: String
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> strOption
      (  long "static-hcd"
      <> short 's'
      <> value ""
      <> metavar "STATIC_WIRES.hcd"
      <> help "Dump all static wires to this HCD (wires that hold the same value always)")
  <*> strOption
      (  long "dynamic-hcd"
      <> short 'd'
      <> value ""
      <> metavar "DYNAMIC_WIRES.hcd"
      <> help "Dump all dynamic wires to this HCD (wires that change value)")
  <*> strOption
      (  long "static-names"
      <> short 'S'
      <> value ""
      <> metavar "STATIC_WIRES.txt"
      <> help "Dump the names of all static wires to this file")
  <*> strOption
      (  long "dynamic-names"
      <> short 'D'
      <> value ""
      <> metavar "DYNAMIC_WIRES.txt"
      <> help "Dump the names of all dynamic wires to this file")
  <*> argument str
      (  metavar "HCDFILE.hcd"
      <> value ""
      <> help  "A Horizontal-Compressed Dump file")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Split wires in an HCD into those that change and those that are static. You can dump HCDs with the static and dynamic wires and dump lists of names. This helps setting up a static port whose vectors can occupy no memory. It also helps you to understand what's going on in your test vectors."
  <> header "hcd-split-static - Split static and dynamic wires")


main :: IO ()
main = do
    Opts staticHcd dynamicHcd staticList dynamicList vcdFile <- execParser opts
    f <- getInput vcdFile

    let
      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map ls2Hcd

      (static, dynamic) = partition isStatic hcds

    unless (null staticHcd)
      ( B.writeFile staticHcd (mkStaticOut static) )
    unless (null dynamicHcd)
      ( B.writeFile dynamicHcd (mkDynamicOut dynamic) )
    unless (null staticList)
      ( B.writeFile staticList (mkPinList static) )
    unless (null dynamicList)
      ( B.writeFile dynamicList (mkPinList dynamic) )
