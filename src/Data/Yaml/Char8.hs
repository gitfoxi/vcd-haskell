{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    OverloadedStrings, UndecidableInstances,
    ViewPatterns, IncoherentInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Isolate the orphan instance in this file

module Data.Yaml.Char8
 (module Yaml)
  where

import           Data.Aeson.Types (typeMismatch)
import           Data.ByteString.Char8
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Yaml as Yaml

instance FromJSON ByteString where
  parseJSON (Yaml.String t) = return $ encodeUtf8 t
  parseJSON invalid = typeMismatch "String" invalid
  {-# INLINE parseJSON #-}

instance ToJSON ByteString where
  toJSON bs = String $ decodeUtf8 bs
  {-# INLINE toJSON #-}

instance (FromJSON v) => FromJSON (M.Map ByteString v) where
    parseJSON = fmap (hashMapKey (pack . T.unpack)) . parseJSON
    {-# INLINE parseJSON #-}

-- | Transform a 'M.Map' into a 'H.HashMap' while transforming the keys.
hashMapKey :: (Ord k2) => (k1 -> k2)
           -> H.HashMap k1 v -> M.Map k2 v
hashMapKey kv = H.foldrWithKey (M.insert . kv) M.empty
{-# INLINE hashMapKey #-}

