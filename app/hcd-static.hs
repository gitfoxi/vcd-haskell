{- hcd-static < pattern.hcd > static.hcd

Convert and hcd that only contains static pins to a 1-cycle hcd to save memory
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B

import Lib

isStatic :: Hcd -> Bool
isStatic (Hcd _ l _) =
  let ws = B.words l
  in
    (not . null) ws && (null . tail) ws

mkStaticOut :: [Hcd] -> ByteString
mkStaticOut hcds =
  map toByteString hcds
  & B.concat

main :: IO ()
main = do
    f <- B.getContents

    let
      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map fromList

      nonStatic = filter (not . isStatic) hcds

    unless (null nonStatic)
      $ error ( "Non-static pins detected:\n" ++
                ( B.unpack . B.concat . map toByteString
                  $ nonStatic ) )

    B.putStr (mkStaticOut hcds)
