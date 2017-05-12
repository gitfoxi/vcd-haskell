{- v-clock < pattern.vcd

-- Clocks in signals on rising jtg_tck

-- TODO clock outputs on falling tck
-- TODO change timestamp to 200ns
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HashMap

import Lib


getAlias (Wire _ alias _) = alias

keepLast :: Hashable b => Eq b => (a -> b) -> [a] -> [a]
keepLast f as = HashMap.elems . HashMap.fromList $ zip (map f as) as

stateChangeAlias = B.tail

stateChangeState :: ByteString -> Char
stateChangeState sc
  | B.null sc = 'x'
  | otherwise = B.head sc

hasAlias alias sc =
  alias == stateChangeAlias sc

isState st sc =
  st == stateChangeState sc

groupUntil = split . keepDelimsR . whenElt

interleve :: [a] -> [a] -> [a]
interleve (a:as) (b:bs) = a:b:interleve as bs
interleve _ [] = []
interleve [] _ = []

enlist :: [a] -> [[a]]
enlist ( a:as ) = [a]:enlist as

clock :: B.ByteString -> B.ByteString -> B.ByteString
clock tck_alias =
  let
    -- group state changes in each timestamp discarding the timestamp
    groupByTime = splitWhen isTimestamp
    uniquify = keepLast stateChangeAlias
    tckRise = any (\sc -> hasAlias tck_alias sc && isState '1' sc)
    groupByTckRise = groupUntil tckRise
    interleveTimestamps = interleve (enlist ascendingTimestamps )
    ascendingTimestamps = zipWith B.cons (repeat '#') (map (B.pack . show) [0..])
  in
    B.unlines .
    concat .
    interleveTimestamps .
    map uniquify . -- [[BS]]
    map concat . -- [[BS]] grouped by time
    groupByTckRise . -- [[[BS]]]
    groupByTime . -- [[BS]]
    B.lines -- [BS]

main :: IO ()
main = do

    f <- B.getContents

    let
        (hdrs, theRest) = splitHeaders f

    let
        isTckWire (Wire _ _ nm) = nm ==  "jtg_tck"
        isTckWire x = False
        -- TODO: ERROR if not exactly one match
        tck_alias = getAlias ( head (filter isTckWire (flattenHeaders hdrs )) )



    hPutBuilder stdout (foldMap render hdrs)
    -- XXX Ugly hack, should add a top-level header to contain the others
    B.putStrLn "$enddefinitions $end"
    -- TODO: probably won't stream because has to read the whole input before beginning output
    B.putStr (clock tck_alias theRest)

