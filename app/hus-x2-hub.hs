{- x2

   Map an expanded, transposed (horizontal, if you will) file like

    PINA XXHHHHLLL
    PINB 01101Z010

To binary bytes as given by the wavetable

TODO This is a bit slower than I'd like
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as Map

import Lib

waveTable :: HashMap ByteString Char
waveTable
  = Map.fromList $
      zip
        [B.pack [a, b] | a <- states, b <- states]
        (map chr [0..255])

defaultWave :: Char
defaultWave = Map.lookupDefault (chr 255) "xx" waveTable

statesMap :: ByteString -> ByteString
statesMap = B.pack . go
  where
    go bs
      | B.null bs = []
      | otherwise =
      let
        (s, rest) = B.splitAt 2 bs
      in
        Map.lookupDefault defaultWave s waveTable : go rest

xForm :: ByteString -> ByteString
xForm inp =
  let
    [pin, statesIn] = B.words inp
    statesOut = statesMap . pad 2 . canonical $ statesIn
  in B.unwords
    [ pin
    , B.pack $ show ( B.length statesOut )
    , statesOut
    ]

data Opts =
  Opts
  { repeatsOutFile :: FilePath
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> argument str
      (  metavar "HUSFILE.hus"
      <> value ""
      <> help "A Horizontal-Uncompressed State file (or use stdin)")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Map states in a Horizontal-Uncompressed State file to the standard X2 wavetable and output Horizontal-Uncompressed Binary"
  <> header "hus-x2-hub - apply standard X2 wavetable")

main :: IO ()
main = do
  Opts inpFile <- execParser opts
  contents <- getInput inpFile

  mapM_
    (B.putStrLn
    . xForm
    )
    (B.lines contents)
