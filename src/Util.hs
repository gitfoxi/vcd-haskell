{- Util.hs

Some function used by more than one script.
-}

{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.HashSet as Set
import           Data.HashSet ( HashSet )
import           System.IO

import Vcd

splitHeaders :: ByteString -> ([Header], ByteString)
splitHeaders f =
    let res = parse parseAllHeaders f
        (hdrs, theRest) =
          case res of
            Fail unconsumed contexts message ->
                error $ "VCD header parse failed: "
                     ++ message
                     ++ "\n---- Contexts ----\n"
                     ++ unlines contexts
                     ++ "\n------------------\n"
                     ++ "\nUnconsumed:\n" ++ Prelude.take 200 (B.unpack unconsumed)
                     ++ "\n...\n"
            Partial _ ->
                error $ "VCD header parse failed: "
                     ++ "EOF during header"
            Done theRest' hdrs' -> (hdrs', theRest')
        in (hdrs, theRest)

-- isTimestamp line
--
-- True iff line represents a timestamp
-- {-# INLINE isTimestamp #-}
isTimestamp :: ByteString -> Bool
isTimestamp a = "#" `B.isPrefixOf` a



flattenHeaders :: [Header] -> [Header]
flattenHeaders ( (Scope _ hs):hss ) = flattenHeaders hs ++ flattenHeaders hss
flattenHeaders (h:hss) = h:flattenHeaders hss
flattenHeaders [] = []

-- isWire header
-- True iff header is a Wire
isWire :: Header -> Bool
isWire Wire{} = True
isWire _             = False

-- warn message
--
-- Print a warning message on stderr
warn :: String -> IO ()
warn s = hPutStrLn stderr $ "Warning: " ++ s

states :: String
states = "01xlh"

statesSet :: HashSet Char
statesSet = Set.fromList states

canonical :: ByteString -> ByteString
canonical = B.map f
  where
    f :: Char -> Char
    f a | a `Set.member` statesSet = a
        | otherwise = 'x'

pad :: Int -> ByteString -> ByteString
pad n bs =
  let
    n' = fromIntegral n
    l' = fromIntegral $ B.length bs :: Double
    padLen = n * ceiling (l' / n' ) - B.length bs
  in
    B.concat [bs, B.replicate padLen (B.last bs)]

getInput :: FilePath -> IO ByteString
getInput f =
  if f == ""
  then B.getContents
  else B.readFile f

diff :: [Int] -> [Int]
diff (a:b:rest) = b - a : diff (b:rest)
diff _ = []
