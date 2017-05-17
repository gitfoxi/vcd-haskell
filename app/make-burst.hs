{- hcd-split portA.hcd portApins.txt portB.hcd portBpins.txt ... -i all.hcd

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

mkPairs :: [String] -> [(String, String)]
mkPairs ( a:b:rest ) = (a,b) : mkPairs rest
mkPairs _ = []

makePortBurst :: String -> [String] -> String
makePortBurst burst ps =
  unlines . map mk1 . mkPairs $ ps
 where
    mk1 (port,label) =
      "DMAS SQPG,SM,2,(" ++ port ++ ") \n\
\SQLB \"" ++ burst ++ "\",MPBU,0,1,,(" ++ port ++ ") \n\
\SQPG 0,CALL,,\"" ++ label ++ "\",,(" ++ port ++ ") \n\
\SQPG 1,BEND,,,,(" ++ port ++ ")"


data Opts =
  Opts
  { optBurstName :: String
  , optPortPatternList :: [String]
  }

parseOpts :: OptionsParser Opts
parseOpts = Opts
  <$> argument str
      (  metavar "BURST_NAME"
      <> help  "Label name for the multiport burst")

  <*> some (argument str
      (  metavar "PORT_A LABEL_A ..."
      <> help  "Port, label pairs"))

opts :: ParserInfo Opts
opts = info (parseOpts <**> helper)
  ( fullDesc
  <> progDesc "Make a multiport burst from a list of ports and patterns"
  <> header "make-burst - make a multiport burst")


main :: IO ()
main = do
    Opts burst portPinList <- execParser opts

    putStrLn "hp93000,vector,0.1"
    putStr $ makePortBurst burst portPinList
