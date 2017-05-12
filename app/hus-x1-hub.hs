{- Map an expanded, transposed (horizontal, if you will) file like

    PINA XXHHHHLLL
    PINB 01101Z010

To binary bytes as given by the wavetable

TODO: Warn if lookupDefault ever actually takes the default
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)

import Lib

waveTable :: [(Char,Char)]
waveTable
  = map (second chr)
    [('0',0)
    ,('1',1)
    ,('Z',2)
    ,('X',2)
    ,('L',3)
    ,('H',4)
    ,('z',2)
    ,('x',2)
    ,('l',3)
    ,('h',4)
    ]

defaultWave :: Char
defaultWave = chr 255

lookupDefault :: Char -> [(Char, Char)] -> Char -> Char
lookupDefault def table x
  = fromMaybe def (lookup x table)

xForm :: ByteString -> ByteString
xForm inp =
  let [pin, states] = B.words inp
  in B.unwords
    [ pin
    , B.pack $ show ( B.length states )
    , B.map (lookupDefault defaultWave waveTable) states]

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
  -- TODO Wavetable not formatted right by optparse
  <> progDesc "Map states in a Horizontal-Uncompressed State file to the standard X1 wavetable and output Horizontal-Uncompressed Binary. \n\
              \ \n\
              \ Wavetable: \n\
              \ 0 '0' \n\
              \ 1 '1' \n\
              \ 2 'x' or 'z' \n\
              \ 3 'l' \n\
              \ 4 'h'"
  <> header "hus-x1-hub - apply standard X1 wavetable")

main  :: IO ()
main = do
  Opts inpFile <- execParser opts
  contents <- getInput inpFile

  mapM_
    (B.putStrLn
    . xForm
    )
    (B.lines contents)
