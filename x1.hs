{- Map an expanded, transposed (horizontal, if you will) file like

    PINA XXHHHHLLL
    PINB 01101Z010

To binary bytes as given by the wavetable

TODO: Warn if lookupDefault ever actually takes the default
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (chr)
import           Data.Maybe (fromMaybe)
import           System.Environment (getArgs)

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

main :: IO ()
main = do
  contents <- B.getContents

  mapM_
    (B.putStrLn
    . xForm
    )
    (B.lines contents)
