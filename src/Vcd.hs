
{-# LANGUAGE OverloadedStrings #-}

module Vcd
    ( Header (..)
    , Signal (..)

    , filterHeaders
    , mapHeaders
    , parseAllHeaders
    , parseSignal
    , render
    ) where

-- Another attempt at VCD parsing

import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
-- Lazy or Char8? Is there a Lazy.Char8?
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8
                    hiding (takeWhile, parseTest, parse, Fail, Done, takeTill, inClass,
                            takeWhile1, satisfy)
-- import           Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Builder
import           Data.Foldable                        (foldMap)
import           Data.Monoid
import           Data.Word (Word8)

import Prelude hiding (takeWhile)


-- TODO: Define structure separately from contents. This is basically a rosetree that can contain certain things
-- TODO: a Top type and an Unknown type to preserve unrecognized statements
data Header
    = Comment !BS.ByteString
    | TimeScale !Int !BS.ByteString -- FemptoSeconds
    | Scope !BS.ByteString [Header]
    | Wire !Int !BS.ByteString !BS.ByteString -- ^ Width Alias Name
    | Ignored
    deriving (Eq, Show)

-- TODO: may need
--   Headers [Header]
-- instance Traversable Header where

render :: Header -> Builder
render (Comment bs) = mconcat . map byteString $ ["$comment ", bs, "$end\n"]
render (TimeScale i s) = mconcat [byteString "$timescale ", intDec i, byteString s, byteString " $end\n"]
render (Wire i a b) = mconcat [byteString "$var wire ", intDec i, byteString " ", byteString a, byteString " ", byteString b, byteString " $end\n"]
render Ignored = byteString ""
render (Scope a hs) = byteString "$scope module " <> byteString a <> " $end\n"  <> foldMap render hs <> byteString "$upscope $end\n"

-- TODO: Why am I specializing filter for headers? Use a Foldable/Traversable structure
filterHeaders :: (Header -> Bool) -> [Header] -> [Header]
filterHeaders _ [] = []
filterHeaders f ((s@(Scope nm sh)):hss)
  | f s = Scope nm (filterHeaders f sh) : filterHeaders f hss
  | otherwise = filterHeaders f hss
filterHeaders f (a:as)
  | f a = a : filterHeaders f as
  | otherwise = filterHeaders f as

-- TODO: Why am I specializing map for headers? Use a Foldable/Traversable structure
mapHeaders :: (Header -> Header) -> [Header] -> [Header]
mapHeaders _ [] = []
mapHeaders f ((s@(Scope nm sh)):hss)
  = Scope nm (mapHeaders f sh) : mapHeaders f hss
mapHeaders f (a:as)
  = f a : mapHeaders f as


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
    scale <- takeWhile (not . isSpace_w8) -- fs, ns, etc
    skipSpace
    keyword "$end"
    return $ TimeScale n scale

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
