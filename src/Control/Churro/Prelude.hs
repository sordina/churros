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

-- Runners

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
