{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

-- | Common transport-agnostic functions for using Churro.
-- 
module Control.Churro.Prelude where

import Control.Churro.Types

import Prelude hiding (id, (.))

import           Control.Arrow            (arr)
import           Control.Category         (id, (.), (>>>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (waitAny, Async, wait)
import           Control.Exception        (Exception, SomeException, try)
import           Control.Monad            (replicateM_, when)
import           Data.Foldable            (for_)
import           Data.Maybe               (isJust)
import           Data.Time                (NominalDiffTime)
import           Data.Void                (Void)
import           GHC.Natural              (Natural)


-- $setup
-- 
-- The examples in this module require the following imports:
-- 
-- >>> :set -XBlockArguments
-- >>> import Control.Churro.Transport
-- >>> import Data.Time.Clock
-- 

-- * Runners

-- | Automatically wait for a churro to complete.
-- 
runWait :: Transport t => Churro a t Void Void -> IO a
runWait x = wait =<< run x

-- | Read the output of a Churro into a list.
-- 
-- Warning: This will block until the Churro terminates,
--          Accumulating items in memory.
--          Only use when you expect a finite amount of output.
--          Otherwise consider composing with a Sink and using `runWait`.
-- 
-- >>> runWaitListChan $ sourceList [0..4] >>> arr succ
-- [1,2,3,4,5]
-- 
runWaitList :: (Transport t, Monoid a) => Churro a t Void b -> IO [b]
runWaitList c = runWait $ (c >>> arr' (:[])) >>>> sink 

-- | Run a sourced and sinked (double-dipped) churro and return an async action representing the in-flight processes.
--
run :: Transport t => Churro a t Void Void -> IO (Async a)
run = run'

-- | Run any churro, there is no check that this was spawned with a source, or terminated with a sink.
--   This is unsafe, since the pipeline may not generate or consume in a predictable way.
--   Use `run` instead unless you are confident you know what you're doing.
-- 
run' :: Transport t => Churro a t i o -> IO (Async a)
run' c = do
    (_i,_o,a) <- runChurro c
    return a

-- * Library

-- ** Sources

-- | A single items source.
--
-- >>> runWaitChan $ sourceSingleton 13 >>> sinkPrint
-- 13
--
-- Equivalent to `pure` from `Applicative`. Redefined here in case you're looking for a source!
-- 
-- >>> runWaitChan $ pure 23 >>> sinkPrint
-- 23
sourceSingleton :: Transport t => o -> Churro () t Void o
sourceSingleton x = sourceList [x]

-- | Create a source from a list of items, sending each down the churro independently.
--
-- >>> runWaitChan $ sourceList [4,2] >>> sinkPrint
-- 4
-- 2
sourceList :: (Transport t, Foldable f) => f o -> Churro () t Void o
sourceList = sourceIO . for_

-- | Create a source from an IO action that is passed a function to yield new items.
--
-- >>> runWaitChan $ sourceIO (\cb -> cb 4 >> cb 2) >>> sinkPrint
-- 4
-- 2
sourceIO :: Transport t => ((o -> IO ()) -> IO a) -> Churro a t Void o
sourceIO cb =
    buildChurro \_i o -> do
        r <- cb (yeet o . Just)
        yeet o Nothing
        return r

-- | Combine a list of sources into a single source.
-- 
-- Sends individual items downstream without attempting to combine them.
-- 
-- Warning: Passing an empty list is unspecified.
-- 
-- TODO: Use NonEmptyList instead of []
-- 
-- >>> runWaitChan $ sources_ [pure 1, pure 1] >>> sinkPrint
-- 1
-- 1
sources :: (Transport t, Monoid a) => [Source a t o] -> Source () t o
sources ss = sourceIO \cb -> do
    asyncs <- mapM (\s -> run $ s >>>> sinkIO cb) ss
    (_, r) <- waitAny asyncs
    return r

-- | This is `sources` specialised to `()`.
sources_ :: Transport t => [Source () t o] -> Source () t o
sources_ = sources

-- ** Sinks

-- | Consume all items with no additional effects.
-- 
-- TODO: Decide if we should use some kind of `nf` evaluation here to force items.
-- 
-- >>> runWaitChan $ pure 1 >>> process print >>> sink_
-- 1
-- 
sink_ :: Transport t => Churro () t i Void
sink_ = sinkIO (const (return ()))

-- | Consume all items and combines them into a result via their monoid.
-- 
-- >>> :set -XFlexibleContexts
-- >>> r <- runWaitChan $ pure' [1 :: Int] >>> sink
-- >>> print r
-- [1]
sink :: (Transport t, Monoid a) => Churro a t a Void
sink = sinkIO return

-- | Consume a churro with an IO process.
-- 
-- >>> runWaitChan $ pure 1 >>> sinkIO (\x -> print "hello" >> print (succ x))
-- "hello"
-- 2
sinkIO :: (Transport t, Monoid a) => (o -> IO a) -> Churro a t o Void
sinkIO cb = buildChurro \i _o -> yankAll i cb

-- | Create a "sink" with more flexibility about when items are demanded using a higher-order "HO" callback.
-- 
-- This also allows a non-unit async action that can be recovered when run.
-- 
-- WARNING: You should use the provided callback if you want to acually create a sink.
-- 
-- TODO: Use hidden callback return type in order to ensure that the callback is called.
-- 
-- >>> import System.Timeout (timeout)
-- >>> :{
-- do
--   r <- timeout 100000 $ runWaitChan $ sourceSingleton 1 >>>> sinkHO \ya -> do
--     ya (print . show)
--     return 25
--   print r
-- :}
-- "1"
-- Just 25
sinkHO :: Transport t => (((i -> IO ()) -> IO ()) -> IO a) -> Churro a t i o
sinkHO cb = buildChurro \i _o -> cb (yankAll i)

-- | Consume and print each item. Used in many examples, but not much use outside debugging!
-- 
-- >>> runWaitChan $ pure "hi" >>> sinkPrint
-- "hi"
sinkPrint :: (Transport t, Show a) => Churro () t a Void
sinkPrint = sinkIO print
    

-- ** Churros

-- | Process each item with an IO action.
--   Acts as a one-to-one process.
-- 
-- >>> runWaitChan $ pure "hi" >>> process (\x -> print x >> return (reverse x)) >>> sinkPrint
-- "hi"
-- "ih"
process :: Transport t => (a -> IO b) -> Churro () t a b
process f = processN (fmap pure . f)

-- | Print each item then pass it on.
processPrint :: (Transport t, Show b) => Churro () t b b
processPrint = process \x -> do print x >> return x

-- | Print each item with an additional debugging label.
processDebug :: (Transport t, Show b) => String -> Churro () t b b
processDebug d = process \x -> putStrLn ("Debugging [" <> d <> "]: " <> show x) >> return x

-- | Process each item with an IO action and potentially yield many items as a result.
--   Acts as a one-to-many process.
-- 
-- >>> runWaitChan $ pure 1 >>> processN (\x -> print (show x) >> return [x, succ x]) >>> sinkPrint
-- "1"
-- 1
-- 2
processN :: Transport t => (i -> IO [o]) -> Churro () t i o
processN f =
    buildChurro \i o -> do
        yankAll i \x -> do mapM_ (yeet o . Just) =<< f x
        yeet o Nothing

-- | Extract xs from (Just x)s. Similar to `catMaybes`.
-- 
-- >>> runWaitChan $ sourceList [Just 1, Nothing, Just 3] >>> justs >>> sinkPrint
-- 1
-- 3
justs :: Transport t => Churro () t (Maybe a) a
justs = mapN (maybe [] pure)

-- | Extract ls from (Left l)s.
-- 
-- >>> runWaitChan $ sourceList [Left 1, Right 2, Left 3] >>> lefts >>> sinkPrint
-- 1
-- 3
lefts :: Transport t => Churro () t (Either a b) a
lefts = mapN (either pure (const []))

-- | Extract rs from (Right r)s.
-- 
-- >>> runWaitChan $ sourceList [Left 1, Right 2, Left 3] >>> rights >>> sinkPrint
-- 2
rights :: Transport t => Churro () t (Either a b) b
rights = mapN (either (const []) pure)

-- | Take and yield the first n items.
-- 
-- WARNING: This is intended to terminate upstream once the items have been consumed
--          downstream, but there is a bug preventing this from working at present!
-- 
-- >>> runWaitChan $ sourceList [1..100] >>> takeC 2 >>> sinkPrint
-- 1
-- 2
-- 
-- This implementation explicitly stops propagating when the Churro completes,
-- although this could be handled by downstream consumer composition terminating
-- the producer and just using replicateM.
takeC :: (Transport t, Integral n) => n -> Churro () t a a
takeC n = buildChurro \i o -> go n i o
    where
    go t i o
        | t <= 0 = yeet o Nothing
        | otherwise = do
            x <- yank i
            yeet o x
            when (isJust x) do go (pred t) i o

-- | Drop the first n items.
-- 
-- >>> runWaitChan $ sourceList [1..4] >>> dropC 2 >>> sinkPrint
-- 3
-- 4
dropC :: (Transport t, Integral n) => n -> Churro () t a a
dropC n = buildChurro \i o -> do
    replicateM_ (fromIntegral n) (yank i) -- TODO: Check the async behaviour of this...
    c2c id i o

-- | Filter items according to a predicate.
--
-- >>> runWaitChan $ sourceList [1..5] >>> filterC (> 3) >>> sinkPrint
-- 4
-- 5
filterC :: Transport t => (a -> Bool) -> Churro () t a a
filterC p = mapN (filter p . pure)

-- | Run a pure function over items, producing multiple outputs.
-- 
-- >>> runWaitChan $ pure 9 >>> mapN (\x -> [x,x*10]) >>> sinkPrint
-- 9
-- 90
mapN :: Transport t => (a -> [b]) -> Churro () t a b
mapN f = processN (return . f)

-- | Delay items from being sent downstream.
-- 
--   Note: NominalDiffTime's Num instance interprets literals as seconds.
-- 
-- >>> let sinkTimeCheck = process (const getCurrentTime) >>> withPrevious >>> arr (\(x,y) -> diffUTCTime y x > 0.01) >>> sinkPrint
-- 
-- >>> runWaitChan $ sourceList [1..2] >>> sinkTimeCheck
-- False
-- 
-- >>> runWaitChan $ sourceList [1..2] >>> delay 0.1 >>> sinkTimeCheck
-- True
delay :: Transport t => NominalDiffTime -> Churro () t a a
delay = delayMicro . ceiling @Double . fromRational . (*1000000) . toRational

-- | Delay items in microseconds. Works the same way as `delay`.
delayMicro :: Transport t => Int -> Churro () t a a
delayMicro d = process \x -> do
    threadDelay d
    return x

-- | Passes consecutive pairs of items downstream.
-- 
-- >>> runWaitChan $ sourceList [1,2,3] >>> withPrevious >>> sinkPrint
-- (1,2)
-- (2,3)
withPrevious :: Transport t => Churro () t a (a,a)
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

-- | Requeue an item if it fails. Swallows exceptions and gives up after retries.
-- 
--  Note: Process will always try once so if retries = 1 then a failing process will execute twice.
-- 
--  The item is requeues on the input side of the churro, so if other items have
--  been passed in they will appear first!
-- 
--  Catches all `SomeException`s. If you wish to narrow the execption type, consider
--  using the processRetry' variant composed with `rights`.
-- 
--  Note: There is an edgecase with Chan transport where a queued retry may not execute
--        if a source completes and finalises before the item is requeued.
--        A different transport type may allow a modified retry function that requeues differently.
-- 
-- >>> :{
-- let
--   prog = processRetry 1 flakeyThing
--   flakeyThing x = do
--     if x > 1
--       then print "GT"  >> return x
--       else print "LTE" >> error ("oops! " <> show x)
-- in
--   runWaitChan $ sourceList [1,2] >>> delay 0.1 >>> prog >>> sinkPrint
-- :}
-- "LTE"
-- "LTE"
-- "GT"
-- 2
-- 
processRetry :: Transport t => Natural -> (i -> IO o) -> Churro () t i o
processRetry retries f = processRetry' @SomeException retries f >>> rights

-- | Raw version of `processRetry`. -- Polymorphic over exception type and forwards errors.
--   
processRetry' :: (Exception e, Transport t) => Natural -> (i -> IO o) -> Churro () t i (Either e o)
processRetry' retries f = arr (0,) >>> processRetry'' retries f

-- | Rawest version of `processRetry`.
--   Expects the incoming items to contain number of retries.
-- 
--   Also polymorphic over exception type. And forwards errors.
--   
processRetry'' :: (Transport t, Exception e, Ord n, Enum n) => n -> (a -> IO b) -> Churro () t (n, a) (Either e b)
processRetry'' retries f =
    buildChurro' \i' o i -> do
        yankAll o \(n, y) -> do
            r <- try do f y
            yeet i (Just r)
            case r of
                Right _ -> return ()
                Left  _ -> when (n < retries) do yeet i' (Just (succ n, y))
        yeet i Nothing
