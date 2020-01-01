{-# language TypeApplications #-}
{-# language OverloadedLists #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language BlockArguments #-}
{-# language PartialTypeSignatures #-}

module Main where

import Data.Foldable
import Data.Function
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List as List
import System.Environment
import System.Process
import System.Random
import System.Random.Shuffle
import qualified Data.Array as Array
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (runSTArray)
import GHC.ST (ST)
import Data.Maybe

initialize :: IO (String, String)
initialize = do
    args <- getArgs
    case args of
        [target, k'] -> do
            u' <- readCreateProcess (proc "aubiotrack" [target]) ""
            return (u', k')
        [k'] -> do
            u' <- getContents
            return (u', k')
        _ -> fail $ "Usage: detect-beat [file] nclasses.\n\
                    \If audio file is given, extract beats. Otherwise, read beats from StdIn."

main :: IO ()
main = do
    (u', k') <- initialize
    let k = read k'
        u = (fmap (read @Double) . lines) u'
        v = delta u
    classes <- kMeans k v
    let period = (avg . List.maximumBy (compare `on` length)) classes
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

kMeans :: (RealFrac a, Enum a) => Int -> [a] -> IO [NonEmpty a]
kMeans k xs
    | length xs < k = fail "Error: less values than classes."
    | otherwise = do
        z <- kRandom k xs
        let z' = fixp (flip classifyByNearest xs . fmap avg) z
        return z'

atLeastOneOfEach :: Int -> Int -> IO [Int]
atLeastOneOfEach k n = do
    gen <- getStdGen
    oneOfEach <- shuffleM [1.. k]
    let anyNumberOfEach = randomRs (1, k) gen
    shuffleM (oneOfEach ++ take (n - k) anyNumberOfEach)

kRandom :: Int -> [a] -> IO [NonEmpty a]
kRandom k xs = do
    rs <- atLeastOneOfEach k (length xs)
    return $ (fmap.fmap) snd . classifyBy ((==) `on` fst) . zip rs $ xs

classifyByNearest :: forall a. (Ord a, Num a) => [a] -> [a] -> [NonEmpty a]
classifyByNearest centroids xs = catMaybes . fmap nonEmpty . Array.elems $ runSTArray do
    v <- newListArray (0, length centroids - 1) (repeat mempty)
    let f :: a -> ST _ ()
        f x = do
            ys <- readArray v i
            writeArray v i (x: ys)
          where
            i = indexOfMinimum (fmap (deviation x) centroids)
    traverse_ f xs
    return v

indexOfMinimum :: Ord a => [a] -> Int
indexOfMinimum xs = let x = List.minimum xs; Just i = List.elemIndex x xs in i

classifyBy :: forall a f. Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
classifyBy eq = List.foldl' f [ ]
  where
    f :: [NonEmpty a] -> a -> [NonEmpty a]
    f [ ] y = [y :| [ ]]
    f (xxs@ (x :| xs): xss) y | x `eq` y  = (y :| (x: xs)): xss
                              | otherwise = xxs: f xss y

converge :: Eq a => [a] -> [a]
converge = convergeBy (==)

fixp :: Eq a => (a -> a) -> a -> a
fixp f = last . converge . iterate f

convergeBy :: (a -> a -> Bool) -> [a] -> [a]
convergeBy _ [ ] = [ ]
convergeBy _ [x] = [x]
convergeBy eq (x: xs@(y: _))
    | x `eq` y = [x]
    | otherwise = x : convergeBy eq xs
