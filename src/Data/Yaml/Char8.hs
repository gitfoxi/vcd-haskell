
-- Isolate the orphan instance in this file

module Data.Yaml.Char8
 (module Yaml)
  where

import           Control.Monad (mzero)
import           Data.Aeson.Types (typeMismatch)
import           Data.ByteString.Char8
import           Data.Text.Encoding (encodeUtf8)
import           Data.Yaml as Yaml

instance FromJSON ByteString where
  parseJSON (Yaml.String t) = return $ encodeUtf8 t
  parseJSON invalid = typeMismatch "String" invalid
