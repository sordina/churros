{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

-- | Recommended entrypoint for the Churro library.
--
--   Contains enough functionality to use the package as intended.
--
--   See README.md for more information.
-- 
module Control.Churro

    ( module Control.Churro

    -- ** Churro Packages

    , module Control.Churro.Types
    , module Control.Churro.Prelude
    , module Control.Churro.Transport

    -- ** Other Packages

    -- *** Re-exported from Control.Category
    , Control.Category.id

    -- *** Re-exported from Control.Arrow
    , Control.Arrow.arr
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.>>>)
    , (Control.Arrow.<<<)
    , (Control.Arrow.&&&)
    , (Control.Arrow.***)

    -- *** Re-exported from Data.Void
    , Void()

    -- *** Re-exported from GHC.Natural
    , Natural()
    )

where

import Control.Churro.Types
import Control.Churro.Prelude
import Control.Churro.Transport

import Data.Void
import GHC.Natural
import Control.Arrow
import Control.Category

million :: Fractional p => p
million = 1e6