{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- | MVar Transport Instance. New items overwrite the current queued item.
-- 
-- Useful in subscription-like contexts where you don't care about outdated values.
-- 
-- This is surprisingly useful since it doesn't block sources, but also doesn't accumulate items.
-- 
-- WARNING: Don't use if you want to ensure that all produced items are consumed!
-- 
module Control.Churro.Transport.MVar.Latest where
    
import Control.Churro.Types
import Control.Churro.Prelude

import Control.Concurrent
import Data.Void

data Latest a where

instance Transport Latest where
    data In  Latest a    = ChanIn  (MVar a)
    data Out Latest a    = ChanOut (MVar a)
    yank (ChanOut c)   = takeMVar c
    yeet (ChanIn  c) v = tryTakeMVar c >> putMVar  c v
    flex = do 
        c <- newEmptyMVar
        return (ChanIn c, ChanOut c)

type ChurroLatest a = Churro a Latest

-- | Convenience function for running a Churro with a MVar backed Latest Transport.
-- 
runWaitLatest :: ChurroLatest a Void Void -> IO a
runWaitLatest = runWait

-- | Convenience function for running a Churro into a List with a MVar backed Latest Transport.
-- 
runWaitListLatest :: ChurroLatest () Void o -> IO [o]
runWaitListLatest = runWaitList