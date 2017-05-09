{- hub-binl < pattern.hub port label wavetable > pattern.binl

hub = Horizontal Uncompressed Binary

States have been converted to binary wavetable entries

TODO Handle padding -- divisible by 6 for VM, 8 for SM
     aldc subprocess does compression

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (isSpace)
import           Data.Function ((&))
import           Data.List.Split (chunksOf)
import           Data.Maybe (fromJust)
import           System.Environment (getArgs)
import           System.Exit
import           System.Process.ByteString

import Util

import Debug.Trace

pad :: Int
pad = 8 -- hardcode for now, 6 for VM

getLength :: ByteString -> Int
getLength s =
  let ls = B.lines s
      ws = B.words (head ls)
  in (fst . fromJust . B.readInt ) (head . drop 1 $ ws)

hubLines :: ByteString -> [(ByteString, ByteString)] -- pin, data
hubLines s =
  let
    (pin, afterPin) = B.break isSpace s
    (num, afterNum) = (fromJust . B.readInt ) (B.dropWhile isSpace afterPin )
    (bin, afterBin) = (B.splitAt num (B.drop 1 afterNum))
  in
    case B.null pin of
      True -> []
      False -> (pin, bin) : hubLines (B.drop 1 afterBin )

vecc :: Int -> (ByteString, ByteString) -> ByteString
vecc paddedLength (pin, dat) =
  let
    datLength = B.length dat
  in
    B.concat ["VECC PARA,SM,0,",
              B.pack . show $ paddedLength,
              ",(", pin, "),#",
              B.pack . show . length . show $ datLength,
              B.pack . show $ datLength, dat]

vecd :: Int -> (ByteString, ByteString) -> ByteString
vecd paddedLength (pin, dat) =
  let
    datLength = B.length dat
  in
    B.concat ["VECD PARA,SM,0,",
              B.pack . show $ paddedLength,
              ",(", pin, "),#",
              B.pack . show . length . show $ datLength,
              B.pack . show $ paddedLength, dat ]

padded :: Int -> ByteString -> ByteString
padded paddedLength inp =
  hubLines inp &
  map (\(pin, dat) ->
         B.concat
         [pin
          , " "
          , B.pack . show $ paddedLength
          , " "
          , dat
          , B.replicate (paddedLength - (B.length dat)) (B.last dat)
          ])
  & B.unlines

{- 
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny action onE = tryAny action >>= either onE return
-}

main :: IO ()
main = do
    [port, label, wavetable] <- fmap (map B.pack ) getArgs

    f <- B.getContents
    let
      length = getLength f
      paddedLength = pad * ( ceiling ( fromIntegral length / fromIntegral pad ) ) :: Int
      paddedDat = padded paddedLength f

    B.putStrLn $ "hp93000,vector,0.1"
    B.putStrLn $ B.concat [ "DMAS MTST,SM,0,(", port, ")" ]
    B.putStrLn $ B.concat [ "DMAS PARA,SM,", B.pack . show $ paddedLength, ",(", port, ")" ]
    B.putStrLn $ B.concat [ "DMAS SQPG,SM,4,(", port, ")" ]
    B.putStrLn $ "STML 1"
    B.putStrLn $ B.concat [ "SQLB \"", label, "\",MAIN,0,3,\"", wavetable, "\",(", port, ")" ]
    B.putStrLn $ B.concat [ "SQLA LBL,\"", label, "\",\"PARA_MEM=SM\"" ]
    B.putStrLn $ B.concat [ "SQPG 0,STVA,0,,SM,(", port, ")" ]
    B.putStrLn $ B.concat [ "SQPG 1,STSA,,,SM,(", port, ")" ]
    -- GENV gives the un-padded length
    B.putStrLn $ B.concat [ "SQPG 2,GENV,", B.pack . show $ length, ",,SM,(", port, ")" ]
    B.putStrLn $ B.concat [ "SQPG 3,STOP,,,,(", port, ")" ]

    -- Compressor subprocess
    e <- try $
        readProcessWithExitCode
          "./v2b-experiment/aldc"
          []
          paddedDat

    (exitCode, fc, ferr) <-
      case e of
              Left e  ->
                do
                  warn $ show (e :: IOException)
                  return (ExitFailure 1, "", B.pack . show $ e)
              Right x -> return x


    when (exitCode == ExitSuccess) $
      mapM_ ( B.putStrLn . vecc paddedLength ) (hubLines fc)

    unless (exitCode == ExitSuccess) $ do
      warn $ show ferr
      warn "hub-binl unable to start aldc subprocess; output will be uncompressed VECD instead of compressed VECC"
      mapM_ ( B.putStrLn . vecd paddedLength ) (hubLines paddedDat)
