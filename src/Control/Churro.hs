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
module Control.Churro

    ( module Control.Churro.Types
    , module Control.Churro.Prelude
    , module Control.Churro.Transport
    , module Control.Arrow
    , module Control.Category
    , Void()
    , Natural(..)
    )

where

import Control.Churro.Types
import Control.Churro.Prelude
import Control.Churro.Transport

import Data.Void
import GHC.Natural
import Control.Arrow
import Control.Category