{- hcd-split-static
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
  & B.unlines

main :: IO ()
main = do
    [staticHcd, dynamicHcd, staticList] <- getArgs

    f <- B.getContents

    let
      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map ls2Hcd

      (static, dynamic) = partition isStatic hcds

    B.writeFile staticHcd (mkStaticOut static)
    B.writeFile dynamicHcd (mkDynamicOut dynamic)
    B.writeFile staticList (mkPinList static)
