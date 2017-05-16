
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib

( module X
, ByteString
, OptionsParser
, chr, isSpace
, hPutBuilder
, HashSet
, getArgs
, stderr, hPutStrLn, stdout
, sort, nub, sortBy, groupBy, partition, List.foldl'
, on, (&)
, keepDelimsR, splitWhen, split, whenElt, chunksOf
, setEmpty, setSingleton, setUnion, setUnions, setNull, setSize, setMember, setInsert, setDelete, setMap, setDifference, setIntersection, setFoldl, setFoldr, setFilter, setToList, setFromList, setToMap, setFromMap
, mapEmpty, mapSingleton, mapUnion, mapUnions, mapNull, mapSize, mapMember, mapInsert, mapDelete, mapMap, mapDifference, mapIntersection, mapFoldl, mapFoldr, mapFilter, mapToList, mapFromList, mapMapMaybe, mapLookup, mapLookupDefault

  )
 where

import Control.Arrow as X
import Control.Concurrent as X
import Control.Exception as X
import Control.Monad as X
import Control.Parallel.Strategies as X
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Char8 (ByteString)
import Data.Char (chr, isSpace)
import Data.Function (on, (&))
import Data.HashMap.Strict as X hiding ( empty, singleton, union, unions, null, size, member, insert, delete, map, difference, intersection, foldl', foldr, filter, toList, fromList, mapMaybe, mapLookup, mapLookupDefault )

import Data.HashSet (HashSet)
import Data.Hashable as X
import Data.List (sort, nub, sortBy, groupBy, partition)
import Data.List.Split (keepDelimsR, splitWhen, split, whenElt, chunksOf)
import Data.Maybe as X
import Data.Semigroup as X hiding (option, diff)
import Debug.Trace as X
import Options.Applicative as X hiding (Parser, option)
import System.Environment (getArgs)
import System.Exit as X
import System.IO (stderr, hPutStrLn, stdout)
import System.IO.MMap as X
import System.Mem as X
import System.Process.ByteString as X

import Chunk as X
import Util as X
import Vcd as X
import WaveTable as X
import Hcd as X

import Data.HashSet as Set
import Data.HashMap.Strict as Map
import Data.List as List

import qualified Options.Applicative as Options
type OptionsParser = Options.Parser

setEmpty = Set.empty
setSingleton = Set.singleton
setUnion = Set.union
setUnions = Set.unions
setNull = Set.null
setSize = Set.size
setMember = Set.member
setInsert = Set.insert
setDelete = Set.delete
setMap = Set.map
setDifference = Set.difference
setIntersection = Set.intersection
setFoldl = Set.foldl'
setFoldr = Set.foldr
setFilter = Set.filter
setToList = Set.toList
setFromList = Set.fromList
setToMap = Set.toMap
setFromMap = Set.fromMap

mapEmpty = Map.empty
mapSingleton = Map.singleton
mapUnion = Map.union
mapUnions = Map.unions
mapNull = Map.null
mapSize = Map.size
mapMember = Map.member
mapInsert = Map.insert
mapDelete = Map.delete
mapMap = Map.map
mapDifference = Map.difference
mapIntersection = Map.intersection
mapFoldl = Map.foldl'
mapFoldr = Map.foldr
mapFilter = Map.filter
mapToList = Map.toList
mapFromList = Map.fromList
mapMapMaybe = Map.mapMaybe
mapLookup = Map.lookup
mapLookupDefault = Map.lookupDefault

