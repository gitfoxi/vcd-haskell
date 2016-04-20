
{-# LANGUAGE OverloadedStrings #-}

module Vcd
    ( Header (..)
    , Signal (..)
    , parseAllHeaders
    , parseSignal
    ) where

-- Another attempt at VCD parsing

import           Control.Applicative ( (<$>), (<|>), (*>), (<*>) )
import           Control.Monad (mzero)
-- Lazy or Char8? Is there a Lazy.Char8?
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8
                    hiding (takeWhile, parseTest, parse, Fail, Done, takeTill, inClass,
                            takeWhile1, satisfy)
-- import           Data.Char (isSpace)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Traversable as Traversable
import           Data.Word (Word8)

import Prelude hiding (takeWhile)

type FemptoSeconds = Int

data Header
    = Comment !BS.ByteString
    | TimeScale !FemptoSeconds
    | Scope !BS.ByteString [Header]
    | Wire !Int !BS.ByteString !BS.ByteString -- ^ Width Alias Name
    | Ignored
    deriving (Eq, Show)

-- TODO: may need
--   Headers [Header]
-- instance Traversable Header where


data Signal
    = TimeStamp !Int
    | Change Word8 BS.ByteString -- ^ State Alias
    | Dollars BS.ByteString -- not sure what to do with $dumpvars etc
    deriving Show

lexeme :: Parser a -> Parser a
lexeme parser = do
    res <- parser
    skipSpace
    return res

keyword :: BS.ByteString -> Parser ()
keyword s = do
    _ <- string s
    skipSpace

parseComment :: Parser Header
parseComment = (Comment . BS.pack)
    <$> (keyword "$comment" *> manyTill anyChar (keyword "$end"))

parseTimeScale :: Parser Header
parseTimeScale = do
    keyword "$timescale"
    n <- decimal
    skipSpace
    keyword "fs" -- TODO: support other units
    keyword "$end"
    return $ TimeScale n

parseScope :: Parser Header
parseScope = do
    keyword "$scope"
    _ <- takeWhile (not . isSpace_w8) -- e.g. "module"
    skipSpace
    name <- takeWhile (not . isSpace_w8)
    skipSpace
    keyword "$end"
    hdrs <- parseHeaders
    keyword "$upscope"
    keyword "$end"
    return $ Scope name hdrs

parseWire :: Parser Header
parseWire = do
    keyword "$var"
    keyword "wire" -- TODO: support non-wires
    keyword "1"    -- TODO: support busses
    wid <- takeWhile (not . isSpace_w8)
    skipSpace
    name <- (filterSpace . BS.pack) <$> manyTill anyChar (keyword "$end")
    return $ Wire 1 wid name

filterSpace :: BS.ByteString -> BS.ByteString
filterSpace = BS.filter (not . isSpace)

parseEndDefinitions :: Parser ()
parseEndDefinitions = do
    keyword "$enddefinitions"
    keyword "$end"
    return ()

skipStupidVar :: Parser Header
skipStupidVar = do
    keyword "$var"
    _ <- (keyword "wire" >> mzero) <|> -- fail on legit wire probably can't arrive here
    -- for faster failing go to end of line
        takeWhile (not . isEndOfLine) >> char '\n'
    -- manyTill anyChar (keyword "$end")
    return Ignored

filterIgnored :: [Header] -> [Header]
filterIgnored = filter (/= Ignored)

parseHeaders :: Parser [Header]
parseHeaders = do
    skipSpace
    h <- many' $ choice 
        [ parseWire
        , parseComment
        , parseTimeScale
        , parseScope
        , skipStupidVar
        ] 
    return $ filterIgnored h

parseAllHeaders :: Parser [Header]
parseAllHeaders = do
    h <- parseHeaders
    parseEndDefinitions
    return h

-- | Parse the body of the Vcd which begins with an initial #timestamp
parseSignal :: Parser [Signal]
parseSignal = do
    skipSpace
    many' $ choice
        [ parseTimeStamp
        , parseChange
        , dollarsGarbage
        ]

dollarsGarbage :: Parser Signal
dollarsGarbage = Dollars <$> lexeme nonSpace

parseTimeStamp :: Parser Signal
parseTimeStamp = TimeStamp <$> (char '#' *> lexeme decimal)

parseChange :: Parser Signal
parseChange = Change <$> satisfy (inClass "01lLhHmMxXzZ") <*> lexeme nonSpace

nonSpace :: Parser BS.ByteString
nonSpace = takeWhile1 (not . isSpace_w8)

