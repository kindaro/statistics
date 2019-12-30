{-# language TypeApplications #-}

module Main where

import Control.Monad
import System.Process
import Data.Function
import Data.List
import System.Environment
import Data.Maybe

getData :: IO [Double]
getData = do
    target <- fmap head getArgs
    raw <- readCreateProcess (proc "aubiotrack" [target]) ""
    let xs = raw & lines & fmap read
    return xs

main :: IO ()
main = do
    xs <- getData
    print (length xs)
    let
        delays = delta xs
        shelfs = unfoldr (fmap diag . removeFarthest) delays
        averageDeviations = (catMaybes . fmap avgDeviation) shelfs
        d2s = (delta . delta) averageDeviations
        Just cutPoint = elemIndex (maximum d2s) d2s
        niceShelf = (last . take cutPoint) shelfs
        Just niceAverage = avg niceShelf
    print (length niceShelf)
    print (niceBpm niceAverage)


niceBpm :: RealFrac a => a -> a
niceBpm = (30 /)

avg :: RealFrac a => [a] -> Maybe a
avg [ ] = Nothing
avg xs = Just $ sum xs / (fromIntegral . length) xs

delta :: Num a => [a] -> [a]
delta (x: x': xs) = x' - x: delta (x': xs)
delta _ = [ ]

diag :: a -> (a, a)
diag x = (x, x)

deviation :: RealFrac a => a -> a -> a
deviation z x = (x - z)^2

avgDeviation :: RealFrac a => [a] -> Maybe a
avgDeviation xs = do
    average <- avg xs
    let deviations = fmap (deviation average) xs
    avg deviations

removeFarthest :: RealFrac a => [a] -> Maybe [a]
removeFarthest [ ] = Nothing
removeFarthest xs = do
    average <- avg xs
    let farthest = maximumBy (compare `on` deviation average) xs
    return $ delete farthest xs
