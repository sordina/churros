{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- | MVar Transport Instance. Blocks when item hasn't been consumed downstream.

module Control.Churro.Transport.MVar where
    
import Control.Churro.Types
import Control.Churro.Prelude

import Control.Concurrent
import Data.Void

instance Transport MVar where
    data In  MVar a = ChanIn  (MVar a)
    data Out MVar a = ChanOut (MVar a)
    yank (ChanOut c) = takeMVar c
    yeet (ChanIn  c) = putMVar  c
    flex = do 
        c <- newEmptyMVar
        return (ChanIn c, ChanOut c)

type ChurroMVar a = Churro a MVar

-- | Convenience function for running a Churro with an MVar Transport.
-- 
runWaitMVar :: ChurroMVar a Void Void -> IO a
runWaitMVar = runWait

-- | Convenience function for running a Churro into a List with an MVar Transport.
-- 
runWaitListMVar :: ChurroMVar () Void o -> IO [o]
runWaitListMVar = runWaitList