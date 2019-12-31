{-# language TypeApplications #-}
{-# language OverloadedLists #-}

module Main where

import System.Process
import Data.Function
import System.Environment
import System.Random.Shuffle
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

getData :: FilePath -> IO (NonEmpty Double)
getData target = do
    raw <- readCreateProcess (proc "aubiotrack" [target]) ""
    raw & lines & fmap read & checkNonEmpty

main :: IO ()
main = do
    target <- fmap head getArgs
    u  <- getData target
    let v = NonEmpty.zip [1..] u
    v' <- (fmap NonEmpty.fromList . shuffleM . NonEmpty.toList) v
    let (train', check') = NonEmpty.splitAt (length v' `div` 2) v'
    train <- checkNonEmpty train'
    check <- checkNonEmpty check'
    let period = linearSlope train
        f = linearFit train
        e = validate check f
    print (period, niceBpm period, e)

sigma :: (Functor f, Foldable f, Num b) => (a -> b) -> f a -> b
sigma f = sum . fmap f

linearSlope :: RealFrac a => NonEmpty (a, a) -> a
linearSlope v = sigma termAbove v / sigma termBelow v
  where
    (xs, ys) = NonEmpty.unzip v
    avgx = avg xs
    avgy = avg ys
    termAbove (x, y) = (x - avgx) * (y - avgy)
    termBelow (x, _) = deviation x avgx

linearIntercept :: RealFrac a => NonEmpty (a, a) -> a
linearIntercept v = avgy - slope * avgx
  where
    (xs, ys) = NonEmpty.unzip v
    avgx = avg xs
    avgy = avg ys
    slope = linearSlope v

linearFit :: RealFrac a => NonEmpty (a, a) -> a -> a
linearFit v x = intercept + slope * x
  where
    slope = linearSlope v
    intercept = linearIntercept v

validate :: RealFrac a => NonEmpty (a, a) -> (a -> a) -> a
validate v f = mse (NonEmpty.zip predictions ys)
  where
    (xs, ys) = NonEmpty.unzip v
    predictions = fmap f xs

niceBpm :: RealFrac a => a -> a
niceBpm = (30 /)

mse :: RealFrac a => NonEmpty (a, a) -> a
mse = avg . fmap (uncurry deviation)

avg :: RealFrac a => NonEmpty a -> a
avg xs = sum xs / (fromIntegral . length) xs

delta :: Num a => [a] -> [a]
delta (x: x': xs) = x' - x: delta (x': xs)
delta _ = [ ]

diag :: a -> (a, a)
diag x = (x, x)

deviation :: RealFrac a => a -> a -> a
deviation z x = (x - z)^(2 :: Int)

avgDeviation :: RealFrac a => NonEmpty a -> a
avgDeviation xs = avg deviations
  where
    deviations = fmap (deviation (avg xs)) xs

checkNonEmpty :: [a] -> IO (NonEmpty a)
checkNonEmpty = maybe (fail "Error: Empty list.") return . nonEmpty
