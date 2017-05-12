
module Hcd where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)


data Hcd =
  Hcd
  { hcdPin :: ByteString
  , hcdLens :: ByteString
  , hcdStates :: ByteString
  }

fromList :: [ByteString] -> Hcd
fromList [p,l,s] = Hcd p l s
fromList x = error $ "Bad hcd:\n" ++ show x

toList :: Hcd -> [ByteString]
toList (Hcd p l s) = [p, l, s]

toByteString :: Hcd -> ByteString
toByteString = B.unlines . toList
