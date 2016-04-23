
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Chunk (chunk) where

import           Data.ByteString.Char8 (ByteString, null, elemIndex)
import           Data.ByteString.Internal (ByteString(PS))
import           Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import           Prelude hiding        (null)

{-# INLINE chunk #-}
chunk :: Int -> ByteString -> [ByteString]
chunk sz bs =
  if null bs
    then []
    else
        let (!first, rest) = takeBreakByte sz '\n' bs
        in first : chunk sz rest

{-# INLINE takeBreakByte #-}
-- combination of take and break so I can do chunks that end at newlines
takeBreakByte :: Int -> Char -> ByteString -> (ByteString, ByteString)
takeBreakByte n c ps@(PS x s l)
    | n <= 0    = ("",ps)
    | n >= l    = (ps,"")
    | otherwise =
       let p = PS x (s + n) (l - n)
       in
         case elemIndex c p of
             Nothing -> (ps,"")
             Just m  -> (unsafeTake (n + m) ps, unsafeDrop (n + m) ps)

