{- hus-jtag-comments

Read a hus and output comments about IR and DR like:

77 DR LSB
122 DR MSB tdi=0x75317531 tdo=0xABCDABCD
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Bits (shift)
import           Data.List (scanl')
import           Numeric (showHex)

import Safe

import JtagStateMachine
import Lib hiding (first, second)

data Opts =
  Opts
  { optInputFile :: FilePath
  , optTms :: String
  , optTdi :: String
  , optTdo :: String
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> argument str
      (  metavar "HUSFILE.hus"
      <> value ""
      <> help "A Horizontal-Uncompressed State file (or use stdin)")
  <*> strOption
      (  long "tms"
      <> short 'm'
      <> value "jtg_tms"
      <> metavar "TMS_PIN"
      <> help "Pin name for JTAG TMS")
  <*> strOption
      (  long "tdi"
      <> short 'i'
      <> value "jtg_tdi"
      <> metavar "TDI_PIN"
      <> help "Pin name for JTAG TDI")
  <*> strOption
      (  long "tdo"
      <> short 'o'
      <> value "jtg_tdo"
      <> metavar "TDO_PIN"
      <> help "Pin name for JTAG TDO")


opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Generate comments based on JTAG state machine"
  <> header "hus-jtag-comments")

findPins :: [String] -> ByteString -> [ByteString]
findPins pins hus =
  let
    candidates = filter valid . map B.words . B.lines $ hus
    valid ps = length ps == 2
    getPin pin = head . tail . headNote
      (error "Pin " <> pin <> " not found in input")
      . filter ((B.pack pin ==) . head) $ candidates
  in
    map getPin pins

jtagIOFromStates :: (ByteString,ByteString,ByteString) -> [JtagIO]
jtagIOFromStates (tmss, tdis, tdos) =
  map fromTriple $
    zip3 (map mapBool . B.unpack $ tmss )
         (map mapMayBool . B.unpack $ tdis )
         (map mapMayBool . B.unpack $ tdos )
  where
    fromTriple (tms, tdi, tdo) = JtagIO tms tdi tdo
    mapBool '0' = False
    mapBool '1' = True
    mapBool  _  = error "State other than 0 1 in TMS"
    mapMayBool '0' = Just False
    mapMayBool '1' = Just True
    mapMayBool 'l' = Just False
    mapMayBool 'h' = Just True
    mapMayBool 'L' = Just False
    mapMayBool 'H' = Just True
    mapMayBool  _  = Nothing

data Dr =
  Dr
  { drIn :: String
  , drOut :: String
  , drIr :: Maybe Integer
  , cycle :: Int
  } deriving Show

-- LSB First
-- For my first crack, just map X to 0
decodeXNum :: [Maybe Bool] -> String
decodeXNum = (\a -> a "") . showHex . foldl' shiftAdd 0 . map xzero . reverse
  where
    xzero Nothing = False
    xzero (Just a) = a

shiftAdd :: Integer -> Bool -> Integer
shiftAdd acc a = shift acc 1 + fromB a

fromB :: Num a => Bool -> a
fromB True = 1
fromB False= 0

decodeDr :: ([(Int, JtagState, JtagIO)], Maybe Integer) -> Maybe Dr
decodeDr (grp, ir)
  | isDr grp = Just $
    Dr (decodeXNum . map (tdi . third) $ grp)
      (decodeXNum . map (tdo . third) $ grp)
      ir
      (first . last $ grp)
  | otherwise = Nothing

first  (a,_,_) = a
second (_,a,_) = a
third  (_,_,a) = a

isState :: JtagState -> [(Int, JtagState, JtagIO)] -> Bool
isState state grp
  | null grp = False
  | (  (== state) . second  . head) grp = True
  | otherwise = False

isDr = isState ShiftDr
isIr = isState ShiftIr

main  :: IO ()
main = do
  Opts inpFile tmsPin tdiPin tdoPin <- execParser opts
  contents <- getInput inpFile

  let
    [tmsStates, tdiStates, tdoStates] =
      findPins [tmsPin, tdiPin, tdoPin]
               contents
    jtagIO = jtagIOFromStates ( tmsStates, tdiStates, tdoStates )
    -- (cycle, jtag-state, jtagIO)
    jtagStates :: [(Int, JtagState, JtagIO)]
    jtagStates = zip3 [0..]
                      (tail $ scanl' jtagStateNext TestLogicReset (map tms jtagIO))
                      jtagIO
    groups :: [[(Int, JtagState, JtagIO)]]
    groups = groupBy ((==) `on` jtagState ) jtagStates
    jtagState :: (Int, JtagState, JtagIO) -> JtagState
    jtagState (_,s,_) = s
    ir = tail $ scanl' decodeIR Nothing groups
    decodeIR currentState grp
      | isIr grp = Just (decodeOneIr grp)
      | otherwise = currentState
    decodeOneIr :: [(Int, JtagState, JtagIO)] -> Integer
    decodeOneIr ios = fromBools . map (fromMaybe (error "TDI not 1 or 0 in SCANIR") . tdi . third) $ ios
    fromBools :: [Bool] -> Integer
    fromBools = foldl' shiftAdd (0 :: Integer) . reverse
    drs = mapMaybe decodeDr (zip groups ir)

  mapM_ print drs
