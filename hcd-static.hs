{- hcd-static < pattern.hcd > static.hcd

Convert and hcd that only contains static pins to a 1-cycle hcd to save memory
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Function ((&))
import           Data.List.Split (chunksOf)

import qualified Hcd
import           Hcd (Hcd(..))

isStatic :: Hcd -> Bool
isStatic (Hcd _ l _) =
  let ws = B.words l
  in
    (not . null) ws && (null . tail) ws

mkStaticOut :: [Hcd] -> ByteString
mkStaticOut hcds =
  map Hcd.toByteString hcds
  & B.concat

main :: IO ()
main = do
    f <- B.getContents

    let
      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map Hcd.fromList

      nonStatic = filter (not . isStatic) hcds

    unless (null nonStatic)
      $ error ( "Non-static pins detected:\n" ++
                ( B.unpack . B.concat . map Hcd.toByteString
                  $ nonStatic ) )

    B.putStr (mkStaticOut hcds)
