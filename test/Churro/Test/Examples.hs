{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

-- | A set of examples of more complicated and problematic Churros.
-- 
module Churro.Test.Examples where

import Data.Map (fromList)
import Data.List

import Prelude hiding (id, (.))

import Control.Churro
import Control.Concurrent.Async (wait)
import Control.Concurrent (MVar, Chan)
import Control.Churro.Transport.MVar ()
import Control.Churro.Transport.MVar.Latest (Latest)

-- $setup
-- 
-- >>> import System.Timeout     (timeout)
-- >>> import Control.Monad      (forever)
-- >>> import Control.Concurrent (threadDelay)

-- ** Tests

-- | Example from readme:
-- 
-- No need to run it, just typecheck it.
readme :: IO ()
readme = do
    runWaitChan             $ sourceList [1::Int ..10] >>> processDebug "after source" >>> delay 1 {- seconds -} >>> arr succ >>> sinkPrint
    (wait =<<)  $ run @Chan $ sourceIO (\cb -> cb (1::Int) >> print "Doing whatever!" >> cb 5) >>> filterC (> 3) >>> sinkIO print

-- | Checks that the IO nature of the churros doesn't duplicate operations.
--   Actions within a pipeline should only occur once no matter how the
--   pipeline is composed.
--
-- >>> runWaitChan linear
-- Debugging [l1]: 1
-- Debugging [l2]: 1
-- Debugging [r1]: 1
-- Debugging [r2]: 1
-- 1 
--
linear :: Transport t => Churro () t Void Void
linear = sourceList [1::Int]
    >>> ((processDebug "l1" >>> processDebug "l2") >>> processDebug "r1" >>> processDebug "r2")
    >>> sinkPrint

-- | A more complicated pipeline exampe involving maps.
-- 
-- >>> runWaitChan pipeline
-- (fromList [(0,0),(1,1)],fromList [(1,1),(2,2)])
-- (fromList [(1,1),(2,2)],fromList [(2,2),(3,3)])
pipeline :: ChurroChan () Void Void
pipeline = sourceList (take 3 maps)
        >>> withPrevious
        >>> takeC (10 :: Int)
        >>> sinkPrint
    where
    maps    = map fromList $ zipWith zip updates updates
    updates = map (take 2) (tails [0 :: Int ..])

-- | Combination of different transports:

mvarTest :: Churro () MVar Void Void
mvarTest = sourceIO @MVar (\cb -> cb (1 :: Int) >> cb 2 >> cb 3) >>> delay 1 >>> sinkPrint

latestTest :: Churro () Latest Void Void
latestTest = sourceIO @Latest (\cb -> cb (1 :: Int) >> cb 2 >> cb 3) >>> delay 1 >>> sinkPrint

-- | Should only output the latest values after sink is free to consume
-- 
-- >>> runWait mvarTest
-- 1
-- 2
-- 3

-- >>> runWait latestTest
-- 1
-- 3

-- | Consumers terminaiting should kill sources from producing.
-- 
-- This can fail in the following scenarios if cancellation isn't implemented correctly:
-- 
-- >>> timeout 150000 $ runWaitChan $ sourceList [1..5] >>> delay 0.1 >>> takeC 1 >>> sinkPrint
-- 1
-- Just ()
-- 
-- Cancells infinite producer with inbuilt delays:
-- 
-- >>> timeout 1500000 $ runWaitChan $ sourceIO (\cb -> forever (cb 1 >> threadDelay 100000)) >>> takeC 1 >>> sinkPrint
-- 1
-- Just ()
-- 
-- Cancells upstream infinite producer with no inbuilt delay:
-- 
-- >>> timeout 2500000 $ runWaitChan $ sourceList [1..] >>> delay 0.1 >>> takeC 1 >>> sinkPrint
-- 1
-- Just ()
-- 
-- Note that the timeout here has to be sufficient for a thread switch to occor and the
-- action be cancelled! See what happens if the timeout is only 1.5s:
-- 
-- >>> timeout 1500000 $ runWaitChan $ sourceList [1..] >>> delay 1 >>> takeC 1 >>> sinkPrint
-- 1
-- Nothing
-- 
-- What should happen is that the Category instance composition of:
-- 
--  ...  delay 1 >>> takeC 1  ...
--  ^^^ PRE ^^^^     ^^^ POST ^^^
-- 
-- When POST terminates it should cancel the computation in PRE.
-- 
-- Failures may be caused by one of the following:
-- 
-- * Nested Async actions don't cascade on cancellation (incorrect finally clauses)
-- * The associativity laws of Category are broken, meaning that cancellation doesn't behave as it should.
-- * Producer blocks cancellation from being requested
-- * Chan is blocked preventing indicating termination to consumers
-- * Infinite source is causing issues (ruled out with this test example)
-- 
-- This is currently working, but tests here should check that the implementation
-- isn't broken.