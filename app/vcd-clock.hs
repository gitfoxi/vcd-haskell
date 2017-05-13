{- v-clock < pattern.vcd

-- Clocks in signals on rising jtg_tck

-- TODO clock outputs on falling tck
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

data Opts =
  Opts
  { clockName :: ByteString
  , newTimeScale :: (Int, ByteString)
  , optInput :: FilePath
  }

decodeTimeScale :: String -> (Int, ByteString)
decodeTimeScale s =
  fromMaybe
    ( error $ "Invalid timescale, " ++ s ++ ". Valid example: 200ns" )
    ( B.readInt . B.pack $ s )

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> (fmap B.pack . strOption)
      (  long "clock"
      <> short 'c'
      <> value "jtg_tck"
      <> metavar "CLOCK_NAME"
      <> help "All signals will be sampled on the rising edge of this clock")
  <*> (fmap decodeTimeScale . strOption)
      (  long "time-scale"
      <> short 't'
      <> value "200ns"
      <> metavar "NUM SCALE"
      <> help "A number and suffix giving the new timescale like 200ns or 10ps. This only affects the 'timescale' statement in the output vcd. Its mostly so you can compare original and clocked VCDs side-by-side in twinwave")
  <*> argument str
      (  metavar "VCDFILE.vcd"
      <> value ""
      <> help  "A Vector-Change Dump file")

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Clock in a VCD so that one #TIMESTAMP = one cycle of the clock. #0 is the first cycle, #1 is the second cycle ... you get it. All signals are sampled on the rising edge of the clock"
  <> header "vcd-clock - clock-sample a VCD")

replaceTimeScale :: (Int, ByteString) -> Header -> Header
replaceTimeScale (nm,sc) (TimeScale _ _) = (TimeScale nm sc)
replaceTimeScale _ x = x

main :: IO ()
main = do

    Opts clockWire timeScale vcdFile <- execParser opts
    f <- getInput vcdFile

    let
        (hdrs, theRest) = splitHeaders f
        hdrsOut = mapHeaders (replaceTimeScale timeScale) hdrs

    let
        isTckWire (Wire _ _ nm) = nm == clockWire
        isTckWire _ = False
        -- TODO: ERROR if not exactly one match
        tck_alias = getAlias ( head (filter isTckWire (flattenHeaders hdrs )) )



    hPutBuilder stdout (foldMap render hdrsOut)
    -- XXX Ugly hack, should add a top-level header to contain the others
    B.putStrLn "$enddefinitions $end"
    -- TODO: probably won't stream because has to read the whole input before beginning output
    B.putStr (clock tck_alias theRest)
