{- Util.hs

Some function used by more than one script.
-}

module Util where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe (fromMaybe)

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

isTimestamp :: ByteString -> Bool
isTimestamp s = fromMaybe False ( fmap (( == '#' ) . fst) ( B.uncons s ) )
