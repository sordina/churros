{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

-- | CSP like library: Arrow interface
--
-- TODO:
-- 
--   * [x] Recovery/Retry capability
--   * [x] Fix await deadlock
--   * [x] Generic Chan functions, then specific newtype
--   * [x] Stop using list functions
--   * [x] Different transport options, buffered, etc.
--   * [ ] Different transports for sections of the graph
--   * [ ] Allow configurable parallelism
--   * [ ] Early termination if downstream consumer completes
-- 
module Control.Churro where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Concurrent
import Control.Category
import Control.Concurrent.Async (cancel, wait, Async, async)
import Data.Void
import Data.Foldable (for_)
import Control.Exception (Exception, SomeException, try)
import GHC.Natural (Natural)
import System.Random
import Control.Monad (replicateM_, when, replicateM)
import Data.Time (NominalDiffTime)
import Data.Map (fromList)
import Data.List (tails)

-- Tests

main :: IO ()
main = do

    runWaitChan $ sourceList [1::Int ..5] >>> delay 0.5 >>> sinkPrint

    c <- newChan
    writeList2Chan c (map Just [1::Int ..10] ++ [Nothing])
    yankAll' c print

    runWaitChan $ sourceList [1::Int ..5] >>> sinkPrint
    runWaitChan $ sourceList [1::Int ..10] >>> delay 0.3 >>> sinkPrint
    runWaitChan test0
    runWaitChan test1
    runWaitChan test2
    runWaitChan test3
    runWaitChan test4
    runWaitChan test5
    runWaitChan (id >>> id)
    runWaitChan id
    runWaitChan $ sourceList [1 :: Int ..5] >>> withPrevious >>> sinkPrint
    runWaitChan ( sourceIO (\cb -> replicateM 3 do cb (5 :: Int))
        >>> delay 1
        >>> arr show
        >>> sinkPrint
        )

    runWaitChan test6
    runWait pipeline
    return ()

test0 :: Transport t => Churro t Void Void
test0 = fmap id id

test1 :: Transport t => Churro t Void Void
test1 = sourceList [1::Int,2] >>> arr succ >>> arr show >>> sinkIO putStrLn

test2 :: Transport t => Churro t Void Void
test2 = sourceList [1::Int ..11] >>> prog
    where
    prog = proc i -> do
        j <- arr succ  -< i
        k <- arr show  -< j
        l <- arr succ  -< j
        m <- arr (> 5) -< j
        n <- process (\x@(_x,_y,z) -> print x >> return z) -< (k,l,m)
        o <- arr not -< n
        sinkPrint -< o

test3 :: Transport t => Churro t Void Void
test3 = sourceList [1 :: Int] >>> process print >>> sinkPrint

test4 :: Transport t => Churro t Void Void
test4 = sourceList [1 :: Int] >>> (process print &&& process print) >>> sinkPrint

test5 :: Transport t => Churro t Void Void
test5 = sourceList [1 :: Int ..10] >>> arr (0 :: Natural,) >>> processRetry' @SomeException 20 flakeyThing >>> sinkPrint
    where
    flakeyThing x = do
        r <- randomRIO (1::Int,10)
        if x < 3 || r > 5
            then return x
            else error ("oops! " <> show x)

test6 :: Transport t => Churro t Void Void
test6 = sourceList [1::Int ..] >>> delay 1 >>> takeC (2::Int) >>> sinkPrint

-- | Checks that the IO nature of the churros doesn't duplicate operation
-- | Should only print:
--
-- >>> runWaitChan test6
-- Debugging [l1]: 1
-- Debugging [l2]: 1
-- Debugging [r1]: 1
-- Debugging [r2]: 1
-- 1 
--
test7 :: Transport t => Churro t Void Void
test7 = sourceList [1::Int]
    >>> ((processDebug "l1" >>> processDebug "l2") >>> processDebug "r1" >>> processDebug "r2")
    >>> sinkPrint

pipeline :: ChurroChan Void Void
pipeline = sourceList (take 10 maps)
        >>> withPrevious
        >>> delay 0.5
        >>> takeC (10 :: Int)
        >>> delay 0.5
        >>> sinkPrint
    where
    maps    = map fromList $ zipWith zip updates updates
    updates = map (take 5) (tails [0 :: Int ..])

-- Runners

runWaitChan :: ChurroChan Void Void -> IO ()
runWaitChan = runWait

runWait :: Transport t => Churro t Void Void -> IO ()
runWait x = wait =<< run x

run :: Transport t => Churro t Void Void -> IO (Async ())
run = run'

-- | This is unsafe, since the pipeline may not be terminated
run' :: Transport t => Churro t i o -> IO (Async ())
run' c = do
    -- Compose an empty sourceList to ensure termination
    (_i,_o,a) <- runChurro (sourceList [] >>> c)
    return a

-- Library

buildChurro :: Transport t => (t (Maybe i) -> t (Maybe o) -> IO ()) -> Churro t i o
buildChurro cb = Churro do
    i <- flex
    o <- flex
    a <- async do cb i o
    return (i,o,a)

sourceList :: (Transport t, Foldable f) => f o -> Churro t Void o
sourceList = sourceIO . for_

sourceIO :: Transport t => ((o -> IO ()) -> IO a2) -> Churro t Void o
sourceIO cb =
    buildChurro \_i o -> do
        cb (yeet o . Just)
        yeet o Nothing

sinkIO :: Transport t => (o -> IO ()) -> Churro t o Void
sinkIO cb = buildChurro \i _o -> yankAll i cb

sinkPrint :: (Transport t, Show a) => Churro t a Void
sinkPrint = sinkIO print

process :: Transport t => (a -> IO b) -> Churro t a b
process f = processN (fmap pure . f)

processPrint :: (Transport t, Show b) => Churro t b b
processPrint = process \x -> print x >> return x

processDebug :: (Transport t, Show b) => String -> Churro t b b
processDebug d = process \x -> putStrLn ("Debugging [" <> d <> "]: " <> show x) >> return x

processN :: Transport t => (a -> IO [b]) -> Churro t a b
processN f =
    buildChurro \i o -> do
        yankAll i \x -> do mapM_ (yeet o . Just) =<< f x
        yeet o Nothing

justs :: Transport t => Churro t (Maybe a) a
justs = mapN (maybe [] pure)

lefts :: Transport t => Churro t (Either a b) a
lefts = mapN (either pure (const []))

rights :: Transport t => Churro t (Either a b) b
rights = mapN (either (const []) pure)

takeC :: (Transport t, Integral n) => n -> Churro t a a
takeC n = buildChurro \i o -> do
    replicateM_ (fromIntegral n) (yank i >>= yeet o)
    yeet o Nothing

mapN :: Transport t => (a -> [b]) -> Churro t a b
mapN f = processN (return . f)

delay :: Transport t => NominalDiffTime -> Churro t a a
delay = delayMicro . ceiling @Double . fromRational . (*1000000) . toRational

delayMicro :: Transport t => Int -> Churro t a a
delayMicro d = process \x -> do
    threadDelay d
    return x

withPrevious :: Transport t => Churro t a (a,a)
withPrevious = buildChurro \i o -> do
    prog Nothing i o 
    yeet o Nothing
    where
    prog x i o = do
        y <- yank i
        case (x,y) of
            (Just x', Just y') -> yeet o (Just (x',y')) >> prog y i o
            (Nothing, Just y') -> prog (Just y') i o
            _                  -> return ()

-- | Requeue an item if it fails.
--   Note: There is an edgecase with Chan transport where a queued retry may not execute
--         if a source completes and finalises before the item is requeued.
processRetry :: Transport t => Natural -> (i -> IO o) -> Churro t i o
processRetry maxRetries f = arr (0,) >>> processRetry' @SomeException maxRetries f >>> rights

processRetry' :: (Exception e, Transport t) => Natural -> (a -> IO o) -> Churro t (Natural, a) (Either e o)
processRetry' maxRetries f =
    buildChurro \i o -> do
        yankAll i \(n, y) -> do
            r <- try do f y
            yeet o (Just r)
            case r of
                Right _ -> return ()
                Left  _ -> when (n >= maxRetries) do yeet i (Just (succ n, y))
        yeet o Nothing

-- Data, Classes and Instances

data Churro t i o = Churro { runChurro :: IO (t (Maybe i), t (Maybe o), Async ()) }

class Transport t where
    flex     :: IO (t a)
    yank     :: t a -> IO a
    yeet     :: t a -> a -> IO ()

instance Transport Chan where
    flex = newChan
    yank = readChan
    yeet = writeChan

type ChurroChan = Churro Chan

yeetList :: (Foldable t1, Transport t2) => t2 a -> t1 a -> IO ()
yeetList t = mapM_ (yeet t)

yankAll :: Transport t => t (Maybe i) -> (i -> IO a) -> IO ()
yankAll c f = do
    x <- yank c
    case x of
        Nothing -> return ()
        Just y  -> f y >> yankAll c f

yankAll' :: Transport t => t (Maybe a) -> (Maybe a -> IO b) -> IO b
yankAll' c f = do
    yankAll c (f . Just)
    f Nothing

instance Transport t => Functor (Churro t a) where
    fmap f c = Churro do
        (i,o,a) <- runChurro c
        o'  <- flex
        a'  <- async do
            c2c f o o'
            wait a
        return (i,o',a')

instance Transport t => Category (Churro t) where
    id    = Churro do async (return ()) >>= \a -> flex >>= \c -> return (c,c,a)
    g . f = Churro do
        (fi, fo, fa) <- runChurro f
        (gi, go, ga) <- runChurro g
        a <- async do c2c id fo gi
        b <- async do
            wait ga
            cancel fa
            wait a
        return (fi, go, b)

instance Transport t => Applicative (Churro t Void) where
    pure x  = sourceList [x]
    f <*> g = buildChurro \_i o -> do
        (_fi, fo, fa) <- runChurro f
        (_gi, go, ga) <- runChurro g

        let
            prog :: IO ()
            prog = do
                fx <- yank fo
                gx <- yank go
                case (fx, gx) of
                    (Just f', Just g') -> (yeet o $ Just (f' g')) >> prog
                    _                  -> return ()

        prog
        yeet o Nothing
        wait fa
        wait ga

instance Transport t => Arrow (Churro t) where
    arr = flip fmap id

    first c = Churro do
        (i,o,a) <- runChurro c
        i'      <- flex
        o'      <- flex

        let go = do
                is <- yank i'
                yeet i (fmap fst is)

                os <- yank o
                yeet o' $ (,) <$> os <*> fmap snd is

                case (is, os) of
                    (Just _, Just _) -> go
                    _ -> return ()

        a' <- async do
            go
            yeet o' Nothing
            wait a

        return (i',o',a')

-- Transport Helpers

c2c :: Transport t => (a1 -> a2) -> t (Maybe a1) -> t (Maybe a2) -> IO ()
c2c f i o = yankAll' i (yeet o . fmap f)

-- Other Helpers

takeUptoAndIncluding :: (a -> Bool) -> [a] -> [a]
takeUptoAndIncluding _ []     = []
takeUptoAndIncluding p (x:xs)
    | p x       = [x]
    | otherwise = x : takeUptoAndIncluding p xs


