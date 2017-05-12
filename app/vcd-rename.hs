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

-- cabal/stack: compile RTS threaded
main :: IO ()
main = do
    [renameFile] <- getArgs
    renameContents <- B.readFile renameFile

    let
      -- XXX Will not handle bad input like a blank line
      toTuple [a, b] = (a, b)
      nameMap =
        HashMap.fromList (map ( toTuple . B.words ) ( B.lines renameContents ))

    f <- B.getContents

    let
        (hdrs, theRest) = splitHeaders f

        hdrsOut =
          mapHeaders (mapNames nameMap) hdrs

    hPutBuilder stdout (foldMap render hdrsOut)
    -- XXX Ugly hack, should add a top-level header to contain the others
    B.putStrLn "$enddefinitions $end"
    -- TODO: probably won't stream because has to read the whole input before beginning output
    B.putStr theRest
