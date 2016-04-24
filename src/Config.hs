
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , Clock(..)
  , Clocks
  , Groups
  , Jtag(..)
  , Pin
  , Pins
  , PinConfig(..)
  , PinConfigs
  , Port(..)
  , Ports
  , SingleEndedOrDiff(..)
  , State(..)
  
  , readConfigFile
  ) where

import           Data.ByteString.Char8 (ByteString, readFile)
import           Data.Map (Map)
import           Data.Yaml.Char8 as Yaml
import           Prelude hiding (readFile)
import           GHC.Generics


type Pin = ByteString
type Pins = [Pin]

type PinConfigs = Map ByteString (Maybe PinConfig)
data PinConfig =
  PinConfig
    { vcd'name :: Maybe ByteString
    , force :: Maybe State
    }
    deriving (Show, Generic)

data State = L | H | D | U | N | Z | X
    deriving (Show, Generic)

type Groups = Map ByteString Pins

-- TODO: Might make some sets optional, adding another Maybe layer or use more Set
data Config =
  Config
    { all'pins :: PinConfigs
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

type Ports = Map ByteString Port
data Port =
  Port
    { period'ns :: !Double
    , sampling'offset'ns :: !Double
    , pins :: [Pin]
    }
    deriving (Show, Generic)

data SingleEndedOrDiff = SingleEnded | Differential
    deriving (Show, Generic)

type Clocks = Map ByteString Clock
data Clock =
    SEClock
    { pin :: Pin
    , clock'period'ns :: !Double
    , rising'edge'ns :: Maybe Double
    }
  | DiffClock
    { p'pin :: Pin
    , n'pin :: Pin
    , clock'period'ns :: !Double
    , rising'edge'ns :: Maybe Double
    }
    deriving (Show, Generic)

instance FromJSON Config
instance FromJSON Clock
instance FromJSON Port
instance FromJSON Jtag
instance FromJSON PinConfig
instance FromJSON State

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
