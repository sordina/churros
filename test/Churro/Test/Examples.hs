{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

-- | A set of examples of more complicated and problematic Churros.
-- 
module Churro.Test.Examples where

import Control.Exception
import Control.Monad
import System.Random
import Data.Map (fromList)
import Data.List

import Prelude hiding (id, (.))

import Control.Churro

-- Tests

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
linear :: Transport t => Churro t Void Void
linear = sourceList [1::Int]
    >>> ((processDebug "l1" >>> processDebug "l2") >>> processDebug "r1" >>> processDebug "r2")
    >>> sinkPrint

-- | A more complicated pipeline exampe involving maps.
-- 
-- >>> runWaitChan pipeline
-- (fromList [(0,0),(1,1)],fromList [(1,1),(2,2)])
-- (fromList [(1,1),(2,2)],fromList [(2,2),(3,3)])
pipeline :: ChurroChan Void Void
pipeline = sourceList (take 3 maps)
        >>> withPrevious
        >>> takeC (10 :: Int)
        >>> sinkPrint
    where
    maps    = map fromList $ zipWith zip updates updates
    updates = map (take 2) (tails [0 :: Int ..])