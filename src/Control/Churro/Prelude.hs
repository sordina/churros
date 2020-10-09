{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

-- | Common transport-agnostic functions for using Churro
-- 
module Control.Churro.Prelude where

import Control.Churro.Types

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Concurrent
import Control.Category
import Control.Concurrent.Async (wait, Async)
import Data.Void
import Data.Foldable (for_)
import Control.Exception (Exception, SomeException, try)
import GHC.Natural (Natural)
import Control.Monad (replicateM_, when)
import Data.Time (NominalDiffTime)

-- $setup
-- 
-- >>> import Control.Churro.Transport
-- 

-- * Runners

-- | Automatically wait for a churro to complete.

runWait :: Transport t => Churro t Void Void -> IO ()
runWait x = wait =<< run x

-- | Run a sourced and sinked (double-dipped) churro and return an async action representing the in-flight processes.

run :: Transport t => Churro t Void Void -> IO (Async ())
run = run'

-- | Run any churro, there is no check that this was spawned with a source, or terminated with a sink.
--   This is unsafe, since the pipeline may not generate or consume in a predictable way.
--   Use `run` instead unless you are confident you know what you're doing.

run' :: Transport t => Churro t i o -> IO (Async ())
run' c = do
    -- Compose an empty sourceList to ensure termination
    (_i,_o,a) <- runChurro (sourceList [] >>> c)
    return a

-- * Library

-- ** Sources

-- | A single items source.
--
-- >>> runWaitChan $ sourceSingleton 13 >>> sinkPrint
-- 13
--
-- equivalent to `pure` from `Applicative`.
-- 
-- >>> runWaitChan $ pure 23 >>> sinkPrint
-- 23
sourceSingleton :: Transport t => o -> Churro t Void o
sourceSingleton x = sourceList [x]

-- | Create a source from a list of items, sending each down the churro independently.
--
-- >>> runWaitChan $ sourceList [4,2] >>> sinkPrint
-- 4
-- 2
sourceList :: (Transport t, Foldable f) => f o -> Churro t Void o
sourceList = sourceIO . for_

-- | Create a source from an IO action that is passed a function to yield new items.
--
-- >>> runWaitChan $ sourceIO (\cb -> cb 4 >> cb 2) >>> sinkPrint
-- 4
-- 2
sourceIO :: Transport t => ((o -> IO ()) -> IO a2) -> Churro t Void o
sourceIO cb =
    buildChurro \_i o -> do
        cb (yeet o . Just)
        yeet o Nothing

-- ** Sinks

-- | Consume a churro with an IO process.
-- 
-- >>> runWaitChan $ pure 1 >>> sinkIO (\x -> print "hello" >> print (succ x))
-- "hello"
-- 2
sinkIO :: Transport t => (o -> IO ()) -> Churro t o Void
sinkIO cb = buildChurro \i _o -> yankAll i cb

-- | Consume and print each item. Used in many examples, but not much use outside debugging!
-- 
-- >>> runWaitChan $ pure "hi" >>> sinkPrint
-- "hi"
sinkPrint :: (Transport t, Show a) => Churro t a Void
sinkPrint = sinkIO print

-- ** Churros

-- | Process each item with an IO action.
--   Acts as a one-to-one process.
-- 
-- >>> runWaitChan $ pure "hi" >>> process (\x -> print x >> return (reverse x)) >>> sinkPrint
-- "hi"
-- "ih"
process :: Transport t => (a -> IO b) -> Churro t a b
process f = processN (fmap pure . f)

-- | Print each item then pass it on.
processPrint :: (Transport t, Show b) => Churro t b b
processPrint = process \x -> do print x >> return x

-- | Print each item with an additional debugging label.
processDebug :: (Transport t, Show b) => String -> Churro t b b
processDebug d = process \x -> putStrLn ("Debugging [" <> d <> "]: " <> show x) >> return x

-- | Process each item with an IO action and potentially yield many items as a result.
--   Acts as a one-to-many process.
-- 
-- >>> runWaitChan $ pure 1 >>> processN (\x -> print (show x) >> return [x, succ x]) >>> sinkPrint
-- "1"
-- 1
-- 2
processN :: Transport t => (a -> IO [b]) -> Churro t a b
processN f =
    buildChurro \i o -> do
        yankAll i \x -> do mapM_ (yeet o . Just) =<< f x
        yeet o Nothing

-- | Extract xs from (Just x)s. Similar to `catMaybes`.
-- 
-- >>> runWaitChan $ sourceList [Just 1, Nothing, Just 3] >>> justs >>> sinkPrint
-- 1
-- 3
justs :: Transport t => Churro t (Maybe a) a
justs = mapN (maybe [] pure)

-- | Extract ls from (Left l)s.
-- 
-- >>> runWaitChan $ sourceList [Left 1, Right 2, Left 3] >>> lefts >>> sinkPrint
-- 1
-- 3
lefts :: Transport t => Churro t (Either a b) a
lefts = mapN (either pure (const []))

-- | Extract rs from (Right r)s.
-- 
-- >>> runWaitChan $ sourceList [Left 1, Right 2, Left 3] >>> rights >>> sinkPrint
-- 2
rights :: Transport t => Churro t (Either a b) b
rights = mapN (either (const []) pure)

-- | Take and yield the first n items.
-- 
-- WARNING: This is intended to terminate upstream once the items have been consumed
--          downstream, but there is a bug preventing this from working at present!
-- 
-- >>> runWaitChan $ sourceList [1..100] >>> takeC 2 >>> sinkPrint
-- 1
-- 2
takeC :: (Transport t, Integral n) => n -> Churro t a a
takeC n = buildChurro \i o -> do
    replicateM_ (fromIntegral n) (yank i >>= yeet o)
    yeet o Nothing

-- | Drop the first n items.
-- 
-- >>> runWaitChan $ sourceList [1..4] >>> dropC 2 >>> sinkPrint
-- 3
-- 4
dropC :: (Transport t, Integral n) => n -> Churro t a a
dropC n = buildChurro \i o -> do
    replicateM_ (fromIntegral n) (yank i) -- TODO: Check the async behaviour of this...
    c2c id i o

-- | Filter items according to a predicate.
--
-- >>> runWaitChan $ sourceList [1..5] >>> filterC (> 3) >>> sinkPrint
-- 4
-- 5
filterC :: Transport t => (a -> Bool) -> Churro t a a
filterC p = mapN (filter p . pure)

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
