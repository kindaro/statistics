{-# language TypeApplications #-}
{-# language OverloadedLists #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Data.Bifunctor
import Data.Foldable
import Data.Function
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Environment
import System.Process
import System.Random
import System.Random.Shuffle

getData :: FilePath -> IO (NonEmpty Double)
getData target = do
    raw <- readCreateProcess (proc "aubiotrack" [target]) ""
    raw & lines & fmap read & checkNonEmpty

main :: IO ()
main = do
    [target, k'] <- getArgs
    let k = read k'
    u <- getData target
    v <- (checkNonEmpty . delta . NonEmpty.toList) u
    classes <- kMeans k v
    let period = avg . List.maximumBy (compare `on` length) . NonEmpty.toList $ classes
    print (period, niceBpm period)

niceBpm :: RealFrac a => a -> a
niceBpm = (30 /)

avg :: RealFrac a => NonEmpty a -> a
avg xs = sum xs / (fromIntegral . length) xs

delta :: Num a => [a] -> [a]
delta (x: x': xs) = x' - x: delta (x': xs)
delta _ = [ ]

deviation :: Num a => a -> a -> a
deviation z x = (x - z)^(2 :: Int)

checkNonEmpty :: [a] -> IO (NonEmpty a)
checkNonEmpty = maybe (fail "Error: Empty list.") return . nonEmpty

kMeans :: forall a. (RealFrac a, Enum a) => Int -> NonEmpty a -> IO (NonEmpty (NonEmpty a))
kMeans k xs
    | k > length xs = fail "Error: less values than classes."
    | otherwise = do

        _use_st_arrays
