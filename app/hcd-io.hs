{- hcd-expand-hus < pattern.hcd > pattern.hus

hus = Horizontal Uncompressed States

-- Clocks in signals on rising jtg_tck

-- TODO clock outputs on falling tck
-- TODO change timestamp to 200ns

This code doesn't stream because it has to read the last line of input before it
can write the first line of output.

Memory and runtime will suffer with large inputs.

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import           Safe (lastMay)

import Lib

data IOCtl =
  IOCtl
  { ctlPin :: ByteString
  , targetPin :: ByteString
  , lowCtlLowTarget :: Char
  , lowCtlHighTarget :: Char
  , lowCtlDefault :: Char
  , highCtlLowTarget :: Char
  , highCtlHighTarget :: Char
  , highCtlDefault :: Char
  }

mkIOCtl [p,c,lm,hm] =
  let
    [ll, lh, ld] = B.unpack lm
    [hl, hh, hd] = B.unpack hm
  in
   IOCtl c p ll lh ld hl hh hd

ls2Hcd [p,l,s] = Hcd p l s

hcdToBS (Hcd p l s) = B.unlines [p, l, s]

integrate :: [Int] -> [Int]
integrate xs = (0 :: Int) : go 0 xs
  where
    go n (a:as) = (n + a) : go (n + a) as
    go n [] = []

lineup :: [(Int,Char)] -> [(Int,Char)] -> [(Int,Char,Char)]
lineup = go '0' '0'
  where
    go sa sb aa@((ta,a):as) bb@((tb,b):bs)
      | ta == tb = (ta,a,b) : go a b as bs
      | ta < tb  = (ta,a,sb) : go a sb as bb
      | otherwise= (tb,sa,b) : go sa b aa bs
    go _ _ [] [] = []
    go sa _  [] bs = map (\(tb,b) -> (tb,sa,b)) bs
    go _  sb as [] = map (\(ta,a) -> (ta, a,sb)) as

applyIO ioCtl target control =
  let
    tTimes = (integrate . map (fst . fromJust . B.readInt) . B.words) (hcdLens target)
    cTimes = (integrate . map (fst . fromJust . B.readInt) . B.words) (hcdLens control)
    -- XXX hack an empty state onto the end to account for the final time and drop it later
    tts = zip tTimes (B.unpack . flip B.snoc '.' .  hcdStates $ target)
    cts = zip cTimes (B.unpack . flip B.snoc '.' .  hcdStates $ control)
    (time,cs,ts) = unzip3 $ lineup cts tts
    -- XXX init hacks out that last state again
    ms = init $ map (applyCtl ioCtl) (zip cs ts)
  in
    Hcd (hcdPin target) (B.unwords . map (B.pack . show) . diff $ time ) (B.pack ms)

data StateClass = Low | High | Other

stateClass :: Char -> StateClass
stateClass '0' = Low
stateClass 'l' = Low
stateClass 'L' = Low
stateClass '1' = High
stateClass 'h' = High
stateClass 'H' = High
stateClass _   = Other

applyCtl :: IOCtl -> ( Char, Char ) -> Char
applyCtl ioCtl (cc, tc) =
  let
    scc = stateClass cc
    stc = stateClass tc
  in
    case (scc, stc) of
      (Low, Low)    -> (lowCtlLowTarget ioCtl)
      (Low, High)   -> (lowCtlHighTarget ioCtl)
      (Low, Other)  -> (lowCtlDefault ioCtl)
      (High, Low)   -> (highCtlLowTarget ioCtl)
      (High, High)  -> (highCtlHighTarget ioCtl)
      (High, Other) -> (highCtlDefault ioCtl)
      otherwise     -> (lowCtlDefault ioCtl)

main :: IO ()
main = do
    [ioFile] <- getArgs
    ff <- B.readFile ioFile

    f <- B.getContents

    let
      hcds :: [Hcd]
      hcds =
        B.lines f &
        chunksOf 3 &
        map ls2Hcd

      ioCtls =
        B.lines ff &
        map B.words &
        map mkIOCtl

      hcdMap = Map.fromList (zip (map hcdPin hcds) hcds)
      ioCtlMap = Map.fromList (zip (map targetPin ioCtls) ioCtls)

      hcdOut =
        map doit hcds

      doit h =
        if Map.member (hcdPin h) ioCtlMap
              then
                let
                  p = hcdPin h
                  ioCtl =
                    fromMaybe (error $ B.unpack p ++ " not found in io-control map")
                          ( Map.lookup p ioCtlMap )
                  ioCtlHcd =
                    fromMaybe (error $ B.unpack (ctlPin ioCtl) ++ " not found in hcd")
                      $ Map.lookup (ctlPin ioCtl) hcdMap
                in
                  applyIO ioCtl h ioCtlHcd
              else h

    mapM_ (B.putStr . hcdToBS) hcdOut
