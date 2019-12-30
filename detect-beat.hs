{-# language TypeApplications #-}

module Main where

import System.Process
import Data.Function
import Data.List
import System.Environment

main :: IO ()
main = do
    target <- fmap head getArgs
    raw <- readCreateProcess (proc "aubiotrack" [target]) ""
    let
        beats = raw & lines & fmap (read @Double)
        deltas = delta beats
        averageDelta = avg deltas
        sortedDeltas = deltas & sortBy (compare `on` (deviation averageDelta)) & reverse
        niceDeltas = take (length sortedDeltas `div` 2) sortedDeltas
        averageNiceDelta = avg niceDeltas
        averageNiceBpm = 30 / averageNiceDelta
    print averageNiceBpm

avg :: RealFrac a => [a] -> a
avg xs = sum xs / (fromIntegral . length) xs

delta :: Num a => [a] -> [a]
delta (x: x': xs) = x' - x: delta (x': xs)
delta _ = [ ]

deviation z x = (x - z)^2
