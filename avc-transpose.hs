-- A simple utility to transpose AVC files
--
-- Not especially fast, but it will help to develop v-binary which will
-- be fast
-- TODO: get repeats which may be non-1 and assoc with vector for instruction generation
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           System.Environment (getArgs)
import System.IO.MMap
import System.Mem

getFormat :: ByteString -> [ByteString]
getFormat =
  takeWhile (B.notElem ';') .
  drop 1 .
  dropWhile (/= "FORMAT") .
  B.words .
  B.unlines .
  dropComments .
  B.lines
  where
    dropComments = filter (not . B.isPrefixOf "#" )

getDataLines :: ByteString -> [ByteString]
getDataLines =
  splitSemi .
  drop 1 .
  dropWhile (B.notElem ';') .
  dropWhile (not . B.isPrefixOf "FORMAT") .
  dropLineComments .
  B.lines
  where
    dropLineComments = filter (not . B.isPrefixOf "#" )
    splitSemi [] = []
    splitSemi s =
      let
        (a,b) = span (B.notElem ';') s
        (c,d) = splitAt 1 b
      in B.concat (a ++ c) : splitSemi d

lineSplit :: Int -> ByteString -> ByteString
lineSplit len inp = B.unlines (splitEvery inp)
  where
    splitEvery s
      | B.length s < len = [ s ]
      | otherwise =
        let
          (a,b) = B.splitAt len s
        in
          a : splitEvery b

transpose :: Int -> Int -> ByteString -> ByteString
transpose nRows nCols inp =
  B.pack [B.index inp (row * nCols + col) | col <- [0..nCols - 1], row <- [0..nRows - 1]]

main = do
    [avcFile] <- getArgs
    f <- mmapFileByteString avcFile Nothing
    performGC

    let 
        dataLines = getDataLines f
        states =
          filter (not . B.null ) .
          concatMap (
          take 1 .
          drop 2 .
          B.words) $
          dataLines
        nCols = B.length . head $ states
        nRows = length states
        allStates = B.concat states
        pins = getFormat f
        outp' = zip pins (B.transpose states)


    putStrLn $ "Rows: " ++ show nRows
    putStrLn $ "Cols: " ++ show nCols
    B.putStr (transpose nRows nCols $! allStates )

    -- mapM_ B.putStrLn outp

    -- forM_
    --   outp'
    --   (\( pin, state ) ->
    --     do
    --       B.putStrLn pin
    --       B.putStrLn (lineSplit 80 state )
    --       B.putStrLn ""
    --     )
