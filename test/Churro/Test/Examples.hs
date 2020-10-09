{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

module Churro.Test.Examples where

import Control.Exception
import Control.Monad
import Control.Concurrent
import System.Random
import Data.Map (fromList)
import Data.List

import Prelude hiding (id, (.))

import Control.Churro

-- Tests

main :: IO ()
main = do

    runWaitChan $ sourceList [1::Int ..5] >>> delay 0.5 >>> sinkPrint

    c <- newChan
    l2c c (map Just [1::Int ..10] ++ [Nothing])
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
test6 = sourceList [1::Int ..10] >>> delay 1 >>> takeC (2::Int) >>> sinkPrint

-- | Checks that the IO nature of the churros doesn't duplicate operation
-- | Should only print:
--
-- >>> runWaitChan test7
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