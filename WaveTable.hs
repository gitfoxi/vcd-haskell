{-# LANGUAGE OverloadedStrings #-}

module WaveTable where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (ord)
import Prelude hiding (takeWhile)

data WaveTable
  = WaveTable
  { pins :: [ByteString]
  , waves :: [ByteString]
  } deriving Show


waveTableParser :: Parser WaveTable
waveTableParser = do
  skipWhile isSpace
  _ <- string "PINS"
  pinz <- many1 pinParser
  wavez <- many1 waveParser
  return (WaveTable pinz wavez)

pinParser :: Parser ByteString
pinParser = do
  _ <- takeWhile ( isHorizontalSpace . fromIntegral . ord)
  takeWhile1 (not . isSpace)

waveParser :: Parser ByteString
waveParser = do
  _ <- takeWhile isSpace
  _ <- decimal :: Parser Int
  _ <- takeWhile isSpace
  takeWhile1 (not . isSpace)

waveTablesParser :: Parser [ WaveTable ]
waveTablesParser = many' waveTableParser

readWaveTables :: FilePath -> IO [WaveTable]
readWaveTables fp = do
  f <- B.readFile fp
  case parseOnly waveTablesParser f of
    Left err -> error err
    Right res -> return res
