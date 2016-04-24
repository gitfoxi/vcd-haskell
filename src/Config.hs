
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , Pin
  , Groups
  , Clocks
  , Clock
  , SingleEndedOrDiff
  
  , readConfigFile
  ) where

import           Data.ByteString.Char8 (ByteString, readFile)
import           Data.Map (Map)
import           Data.Yaml.Char8 as Yaml
import           Prelude hiding (readFile)
import           GHC.Generics


type Pin = ByteString

-- TODO: why can't I use bytestring for a map key?
type Groups = Map String [Pin]

data Config =
  Config
    { all'pins :: [Pin]
    , groups :: Groups
    , data'ports :: Ports
    , clock'ports :: Clocks
    , jtag'port :: Jtag
    }
    deriving (Show, Generic)

data Jtag =
  Jtag
    { tck :: Pin
    , tdi :: Pin
    , tdo :: Pin
    , tms :: Pin
    , mux :: [Pin]
    }
    deriving (Show, Generic)

type Ports = Map String Port
data Port =
  Port
    { period'ns :: Double
    , sampling'offset'ns :: Double
    , pins :: [Pin]
    }
    deriving (Show, Generic)

data SingleEndedOrDiff = SingleEnded | Differential
    deriving (Show, Generic)

type Clocks = Map String Clock
data Clock =
    SEClock
    { pin :: Pin
    , clock'period'ns :: Double
    , rising'edge'ns :: Maybe Double
    }
  | DiffClock
    { p'pin :: Pin
    , n'pin :: Pin
    , clock'period'ns :: Double
    , rising'edge'ns :: Maybe Double
    }
    deriving (Show, Generic)

instance FromJSON Config
instance FromJSON Clock
instance FromJSON Port
instance FromJSON Jtag

instance ToJSON Clock

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
