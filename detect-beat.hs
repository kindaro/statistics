{-# language TypeApplications #-}

module Main where

import Control.Monad
import System.Process
import Data.Function
import Data.List
import System.Environment
import Data.Maybe
import Data.Foldable
import System.Random.Shuffle

getData :: FilePath -> IO [Double]
getData target = do
    raw <- readCreateProcess (proc "aubiotrack" [target]) ""
    let xs = raw & lines & fmap read
    return xs

main :: IO ()
main = do
    target <- fmap head getArgs
    u  <- getData target
    let v = zip [1..] u
    v' <- shuffleM v
    let (train, check) = splitAt (length v' `div` 2) v'
        Just slope = linearSlope train
        f = linearFit train
        Just error = validate check f
    print (slope, error)

sigma :: Num b => (a -> b) -> [a] -> b
sigma f = sum . fmap f

linearSlope :: RealFrac a => [(a, a)] -> Maybe a
linearSlope v = do
    let (xs, ys) = unzip v
    avgx <- avg xs
    avgy <- avg ys
    let termAbove (x, y) = (x - avgx) * (y - avgy)
        termBelow (x, y) = (x - avgx)^2
    return $ sigma termAbove v / sigma termBelow v

linearIntercept :: RealFrac a => [(a, a)] -> Maybe a
linearIntercept v = do
    let (xs, ys) = unzip v
    avgx  <- avg xs
    avgy  <- avg ys
    slope <- linearSlope v
    return $ avgy - slope * avgx

linearFit :: RealFrac a => [(a, a)] -> a -> Maybe a
linearFit v x = do
    slope     <- linearSlope v
    intercept <- linearIntercept v
    return $ intercept + slope * x

validate :: RealFrac a => [(a, a)] -> (a -> Maybe a) -> Maybe a
validate v f = do
    let (xs, ys) = unzip v
    predictions <- traverse f xs
    error <- mse (zip predictions ys)
    return error

niceBpm :: RealFrac a => a -> a
niceBpm = (30 /)

mse :: RealFrac a => [(a, a)] -> Maybe a
mse = avg . fmap (deviation 0 . uncurry (-))

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
