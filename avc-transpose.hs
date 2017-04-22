-- A simple utility to transpose AVC files
--
-- Not especially fast, but it will help to develop v-binary which will
-- be fast
-- TODO: get repeats which may be non-1 and assoc with vector for instruction generation
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Unsafe as B
import           Data.Char (isSpace, chr)
import           System.Environment (getArgs)
import System.IO.MMap
import System.Mem
import System.IO (hPutStrLn, stderr)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ByteString as R

import Data.Word (Word8)

import Data.Vector.Storable.ByteString

import qualified Data.Vector as V

getFormat :: ByteString -> [ByteString]
getFormat =
  takeWhile (B.notElem ';') .
  drop 1 .
  dropWhile (/= "FORMAT") .
  B.words .
  B.unlines .
  dropComments .
  B.lines


dropComments :: [ByteString] -> [ByteString]
dropComments = filter (not . B.isPrefixOf "#" ) . map ( B.dropWhile isSpace ) 

getDataLines :: ByteString -> [ByteString]
getDataLines =
  splitSemi .
  drop 1 .
  dropWhile (B.notElem ';') .
  dropWhile (not . B.isPrefixOf "FORMAT") .
  dropComments .
  B.lines
  where
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
  BS.pack [( B.unsafeIndex inp (row * nCols + col) ) | col <- [0..nCols - 1], row <- [0..nRows - 1]]

splitAtEach :: Int -> ByteString -> [ ByteString ]
splitAtEach i inp
  | B.null inp = []
  | otherwise =
  let (first, rest) = B.splitAt i inp
  in first : splitAtEach i rest

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
        allStates = B.concat states -- states as one long bytestring
        pins = getFormat f
        outp' = zip pins (B.transpose states)

        r = R.fromByteString (R.Z R.:. (nRows::Int) R.:. (nCols::Int)) allStates
        tr = R.transpose r

    u <-  R.computeP tr :: IO (R.Array R.U R.DIM2 Word8)
    let v = R.toUnboxed u
        bs = vectorToByteString . V.convert $ v
        horizStates = (splitAtEach nRows bs )
        outLines = zipWith glue pins horizStates
          where
            glue a b = B.concat [a, " ", b]


    hPutStrLn stderr $ "Rows: " ++ show nRows
    hPutStrLn stderr $ "Cols: " ++ show nCols
    performGC
    mapM_ B.putStrLn outLines

    -- B.putStr (transpose nRows nCols $! allStates )

    -- mapM_ B.putStrLn outp

    -- forM_
    --   outp'
    --   (\( pin, state ) ->
    --     do
    --       B.putStrLn pin
    --       B.putStrLn (lineSplit 80 state )
    --       B.putStrLn ""
    --     )

