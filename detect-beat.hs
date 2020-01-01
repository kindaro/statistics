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
    v <- (checkNonEmpty . delta) u
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

newtype ValueId = ValueId { valueId :: Int } deriving (Eq, Ord, Num, Enum, Random)
newtype ClassId = ClassId { classId :: Int } deriving (Eq, Ord, Num, Enum, Random)

kMeans :: forall a. (RealFrac a, Enum a) => Int -> NonEmpty a -> IO (NonEmpty (NonEmpty a))
kMeans k xs
    | k > length xs = fail "Error: less values than classes."
    | otherwise = do

        let xsMap  -- Resolves a value index to its value.
                = enumerate xs
                    :: Map ValueId a

        maybeInitialClasses <- assignRandom1 (Set.fromList . Map.keys $ xsMap) [1.. ClassId k]
        initialClasses
            <- maybe (fail "Logic error: k seems to be > length xs.") return maybeInitialClasses
                :: IO (Map ValueId ClassId)

        let innerLoop :: Map ValueId ClassId -> Map ValueId ClassId
            innerLoop = selectClosest . makeDistanceMap xsMap . makeCentroidMap xsMap

            fitting :: (ClassId, NonEmpty ValueId) -> (NonEmpty a)
            fitting = fmap (xsMap Map.!) . snd

        return $ (fmap fitting . NonEmpty.fromList . fibers . fixp innerLoop) initialClasses

  where

    -- | Assign random values such that each is used at least once. The idea is to append a random
    -- tail to the rainbow of values and shuffle. Tail gets shuffled twice, but whatever.
    assignRandom1 :: forall k v. (Ord k, Ord v) => Set k -> Set v -> IO (Maybe (Map k v))
    assignRandom1 ks vs
        | length vs > length ks = return Nothing
        | otherwise = do
            rs <- randomSequence
            rs' <- shuffleM (Map.keys vMap ++ take (length ks - length (Map.keys vMap)) rs)
            return . Just $ Map.fromList (zip (Set.toList ks) (fmap (vMap Map.!) rs'))
      where
        vMap :: Map ValueId v
        vMap = enumerate (Set.toList vs)
        randomSequence :: IO [ValueId]
        randomSequence = do
            gen <- getStdGen
            return $ randomRs (minimum (Map.keys vMap), maximum (Map.keys vMap)) gen

    makeCentroidMap :: Map ValueId a -> Map ValueId ClassId -> Map ClassId a
    makeCentroidMap xsMap = Map.fromList . (fmap . fmap) (avg . fitting) . fibers
      where
        fitting :: NonEmpty ValueId -> NonEmpty a
        fitting = fmap (xsMap Map.!)

    makeDistanceMap :: Map ValueId a -> Map ClassId a -> Map (ValueId, ClassId) a
    makeDistanceMap xsMap centroidMap = Map.fromSet f ks
      where
        ks :: Set (ValueId, ClassId)
        ks = Set.fromList (liftA2 (,) (Map.keys xsMap) (Map.keys centroidMap))

        f :: (ValueId, ClassId) -> a
        f (i, j) = deviation (xsMap Map.! i) (centroidMap Map.! j)

    selectClosest :: Map (ValueId, ClassId) a -> Map ValueId ClassId
    selectClosest = fmap (fst . List.minimumBy (compare `on` snd) . Map.toList) . curryMap

curryMap :: forall k1 k2 v. (Ord k1, Ord k2) => Map (k1, k2) v -> Map k1 (Map k2 v)
curryMap = Map.fromList . fmap fitting . classifyBy ((==) `on` (fst . fst)) . Map.toList
  where
    fitting :: NonEmpty ((k1, k2), v) -> (k1, Map k2 v)
    fitting xs@ (((k, _), _) :| _)
                = (k, (Map.fromList . NonEmpty.toList . fmap (bimap snd id)) xs)

enumerate :: (Foldable f, Ord i, Num i, Enum i) => f a -> Map i a
enumerate = Map.fromList . zip [1..] . toList

fibers :: forall k v. (Ord k, Ord v) => Map k v -> [(v, NonEmpty k)]
fibers = fmap fitting . classifyBy ((==) `on` snd) . Map.toList
  where
    fitting :: NonEmpty (k, v) -> (v, NonEmpty k)
    fitting ((k, v) :| kvs) = (v, k :| fmap fst kvs)

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
