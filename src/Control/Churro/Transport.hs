{-# LANGUAGE BlockArguments #-}

-- | Re-exporting Transport instances.
-- 
-- Also includes convenience functions for working directly with transports.
-- 
module Control.Churro.Transport

    ( module Control.Churro.Transport.Chan
    , module Control.Churro.Transport
    )

where

import Control.Churro.Transport.Chan
import Control.Churro.Types

-- | Write a list to a raw Transport.
-- 
-- >>> :set -XTypeApplications
-- >>> import Control.Concurrent.Chan
-- >>> :{
-- do
--   (i,o) <- flex @Chan
--   l2c i (map Just [1,2] ++ [Nothing])
--   yankAll' o print
-- :}
-- Just (Just 1)
-- Just (Just 2)
-- Just Nothing
-- Nothing
-- 
l2c :: (Foldable f, Transport t) => In t (Maybe a) -> f a -> IO ()
l2c c l = mapM_ (yeet c . Just) l >> yeet c Nothing
