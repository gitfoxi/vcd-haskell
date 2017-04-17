
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
-- takeBreakByte
--
-- takeBreakByte n c ps
-- tries to take n bytes from ps, unless ps is shorter than n
-- will take more than n bytes to make sure the last byte is a c
-- returns a pair of (taken, rest) where rest is the unconsumed bytestring
-- combination of take and break so I can do chunks that end at newlines
takeBreakByte :: Int -> Char -> ByteString -> (ByteString, ByteString)
takeBreakByte n c ps@(PS x s l)
    | n <= 0    = ("",ps) -- should never happen
    | n >= l    = (ps,"") -- ps is shorter than the requested n bytes
    | otherwise =
       let p = PS x (s + n) (l - n)
       in
         case elemIndex c p of
             Nothing -> (ps,"")
             Just m  -> (unsafeTake (n + m + 1) ps, unsafeDrop (n + m + 1) ps)

