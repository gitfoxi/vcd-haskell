{- hub-binl < pattern.hub port label wavetable > pattern.binl

hub = Horizontal Uncompressed Binary

States have been converted to binary wavetable entries

TODO Handle padding -- divisible by 6 for VM, 8 for SM
     aldc subprocess does compression

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils

import Lib

padTo :: Int
padTo = 8 -- hardcode for now, 6 for VM

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

vecc2 :: Int -> (ByteString, ByteString) -> IO ByteString
vecc2 paddedLength (pin, dat) = do
  compressed <- aldcEncodeC dat
  let
    datLength = B.length compressed
  return $
    B.concat ["VECC PARA,SM,0,",
              B.pack . show $ paddedLength,
              ",(", pin, "),#",
              B.pack . show . length . show $ datLength,
              B.pack . show $ datLength, compressed]

-- VECC using libaldc.so
-- XXX Stupidly running the program once per line and not even in parallel
vecc :: FilePath -> Int -> (ByteString, ByteString) -> IO ByteString
vecc libAldcPath paddedLength (pin, dat) = do
  let
    datLength = B.length dat
    veccLine compressedDat =
      B.concat ["VECC PARA,SM,0,",
                B.pack . show $ paddedLength,
                ",(", pin, "),#",
                B.pack . show . length . show $ datLength,
                B.pack . show $ datLength, dat]

  -- Compressor subprocess
  e <- try $
      readProcessWithExitCode
        "./v2b-experiment/aldc"
        [libAldcPath]
        (B.unlines [B.unwords [pin, dat]])

  (exitCode, fc, ferr) <-
    case e of
            Left e  ->
              do
                warn $ show (e :: IOException)
                return (ExitFailure 1, "", B.pack . show $ e)
            Right x -> return x


  unless (exitCode == ExitSuccess) $
    error "hub-binl unable to start aldc subprocess; check libaldc.so path"

  return (veccLine (head . hubLines $ fc))


vecd :: Int -> (ByteString, ByteString) -> IO ByteString
vecd paddedLength (pin, dat) = do
  let
    datLength = B.length dat
  return $
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

foreign import ccall unsafe "aldc.h EncodeAldcString"
  c_aldcEncodeCString :: Ptr CChar -> CInt -> Ptr ( Ptr CChar ) -> Ptr CInt -> IO CInt

aldcEncodeC :: ByteString -> IO ByteString
aldcEncodeC inp = do
  outLen <- new (0 :: CInt)
  outStr <- new (fromIntegral 0 :: CChar) >>= new
  r <- B.useAsCStringLen inp $
    \(inp,len) -> do
      let inpLen = CInt (fromIntegral len)
      c_aldcEncodeCString inp inpLen outStr outLen
  s <- peek outStr
  l <- peek outLen
  ret <- B.packCStringLen (s, fromIntegral l)
  free s
  return ret

chooseVecc noCompress libAldcPath =
  if noCompress
     then vecd
     else if null libAldcPath
             then vecc2
             else (vecc libAldcPath)

formatComment :: ByteString -> ByteString -> ByteString
formatComment port bs =
  let (line, comment) = B.break isSpace bs
      -- XXX 40 limitation for old testers. New ones can have 250
      -- TODO warn when you truncate a comment
      comment' = B.take 40 . B.tail $ comment
  in
    "CMNT " <> line <> ",\"" <> comment' <> "\",(" <> port <> ")"

main :: IO ()
main = do
    Opts inputFile sPort sLabel sWavetable commentFile repeatFile libAldcPath noCompress useVM <- execParser opts


    f <- getInput inputFile
    let
      port = B.pack sPort
      label = B.pack sLabel
      wavetable = B.pack sWavetable
      length = getLength f
      paddedLength = padTo * ( ceiling ( fromIntegral length / fromIntegral padTo ) ) :: Int
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

    bs <-  mapM ( ((chooseVecc noCompress libAldcPath) paddedLength )  ) (hubLines paddedDat)

    -- putStrLn makes sure to end encoded lines with \n
    mapM_ B.putStrLn bs
    -- TODO Options to use libaldc or leave uncompressed
    -- TODO Don't use compression when it makes things longer
    -- TODO insert repeats and comments
    unless (null commentFile) $
      do
        comments <- B.readFile commentFile
        mapM_ (B.putStrLn . formatComment port) (B.lines comments)

  {-
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
      warn "hub-binl unable to start aldc subprocess; output will be uncompressed VECD instead of compressed VECC"
      mapM_ ( B.putStrLn . vecd paddedLength ) (hubLines paddedDat)
-}

data Opts =
  Opts
  { optInputFile :: FilePath
  , optPort :: String
  , optLabel :: String
  , optWaveTable :: String
  , optCommentFile :: String
  , optRepeatFile :: String
  , optLibAldcPath :: String
  , optNoCompress :: Bool
  , optUseVM :: Bool
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> argument str
      (  metavar "HUB_FILE.hub"
      <> value ""
      <> help "A Horizontal-Uncompressed Binary file (or use stdin)")
  <*> strOption
      (  long "port"
      <> short 'p'
      <> metavar "TARGET_PORT"
      <> help "Port Name - '@' for non-multiport")
  <*> strOption
      (  long "label"
      <> short 'l'
      <> metavar "LABEL"
      <> help "Pattern label")
  <*> strOption
      (  long "wavetable"
      <> short 'w'
      <> value "wDefault"
      <> metavar "WAVETABLE"
      <> help "Wavetable name so vector editor knows how to render your vector")
  <*> strOption
      (  long "comments"
      <> short 'c'
      <> value ""
      <> metavar "COMMENTS.comments"
      <> help "File containing comments to associate with vector lines like '0 First-Line Comment'")
  <*> strOption
      (  long "repeats"
      <> short 'r'
      <> value ""
      <> metavar "REPEATS.rep"
      <> help "(NOT YET IMPLEMENTED) Repeats generated by a repeat compressor like hub-repeat-compress-hub")
  <*> strOption
      (  long "libaldc"
      <> short 'a'
      <> value ""
      <> metavar "/PATH/TO/libaldc.so"
      <> help "Use libaldc.so to do VECC compression instead of the built-in code - libaldc may be faster and more correct; it's certainly been around longer; the catch is you need to run on a compatible Linux with Advantest ASCII Interface Tools installed")
  <*> switch
      (  long "no-compress"
      <> short 'n'
      <> help "Do not use any VECC compression - this is fast for us but slow when you go to load vectors on the tester")
  <*> switch
      (  long "vector-memory"
      <> short 'v'
      <> help "(NOT YET IMPLEMENTED) Use VM Vector Memory - only for old testers like C400 or PinScale running in compatibility mode")


opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Create 93k SmartScale .binl vector"
  <> header "hub-binl")
