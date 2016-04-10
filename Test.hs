
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

import Vcd

testParseSignal s = do
    let sig = parse parseSignal s
    case sig of
        Fail unconsumed contexts message ->
            error $ "Parse failed: "
                 ++ message
                 ++ "\n---- Contexts ----\n"
                 ++ unlines contexts
                 ++ "\n------------------\n"
                 ++ "\nUnconsumed:\n" ++ Prelude.take 200 (B.unpack unconsumed)
                 ++ "\n...\n"
        Done theRest sigs -> do
            putStrLn "Parse Succeeds"
            print sigs
            putStrLn "Leftover"
            print theRest

main :: IO ()
main = do
    f <- B.readFile "head.vcd"
    let res = parse parseAllHeaders f
    case res of
        Fail unconsumed contexts message ->
            error $ "Parse failed: "
                 ++ message
                 ++ "\n---- Contexts ----\n"
                 ++ unlines contexts
                 ++ "\n------------------\n"
                 ++ "\nUnconsumed:\n" ++ Prelude.take 200 (B.unpack unconsumed)
                 ++ "\n...\n"
        Done theRest hdrs -> do
            putStrLn "Parse Succeeds"
            print hdrs
            testParseSignal theRest
