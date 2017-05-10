{- x2

   Map an expanded, transposed (horizontal, if you will) file like

    PINA XXHHHHLLL
    PINB 01101Z010

To binary bytes as given by the wavetable

TODO This is a bit slower than I'd like
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (chr)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import           Data.HashSet ( HashSet )

states :: String
states = "01xlh"

statesSet :: HashSet Char
statesSet = Set.fromList states

waveTable :: HashMap ByteString Char
waveTable
  = Map.fromList $
      zip
        [B.pack [a, b] | a <- states, b <- states]
        (map chr [0..255])

defaultWave :: Char
defaultWave = Map.lookupDefault (chr 255) "xx" waveTable

pad :: Int -> ByteString -> ByteString
pad n bs =
  let
    n' = fromIntegral n
    l' = fromIntegral $ B.length bs :: Double
    padLen = ceiling (l' / n' ) - B.length bs
  in
    B.concat [bs, B.replicate padLen (B.last bs)]

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

canonical :: ByteString -> ByteString
canonical = B.map f
  where
    f :: Char -> Char
    f a | a `Set.member` statesSet = a
        | otherwise = 'x'

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

main :: IO ()
main = do
  contents <- B.getContents

  mapM_
    (B.putStrLn
    . xForm
    )
    (B.lines contents)
