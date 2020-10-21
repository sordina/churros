{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- | MVar Transport Instance. New items overwrite the current queued item.
-- 
-- Useful in subscription-like contexts where you don't care about outdated values.

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

-- | Convenience function for running a Churro with a Chan Transport.
-- 
runWaitChan :: ChurroLatest a Void Void -> IO a
runWaitChan = runWait

-- | Convenience function for running a Churro into a List with a Chan Transport.
-- 
runWaitListChan :: ChurroLatest () Void o -> IO [o]
runWaitListChan = runWaitList