
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , Pin
  , Groups
  
  , readConfigFile
  ) where

import           Data.ByteString.Char8 (ByteString, readFile)
import           Data.Map (Map)
import           Data.Yaml.Char8 as Yaml
import           Prelude hiding (readFile)
import           GHC.Generics


type Pin = ByteString

-- TODO: why can't I use bytestring for group name?
type Groups = Map String [Pin]

data Config =
  Config
    { pins :: [Pin]
    , groups :: Groups
    }
    deriving (Show, Generic)

instance FromJSON Config
readConfigFile :: FilePath -> IO Config
readConfigFile configFile = do
    configFileContents <- readFile configFile
    let
        config :: Config
        config =
         case decodeEither' configFileContents of
          Right c -> c
          Left e -> error $ configFile ++ ": " ++ show e
    return config
