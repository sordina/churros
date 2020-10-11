{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- | Chan Transport Instance.

module Control.Churro.Transport.Chan where
    
import Control.Churro.Types
import Control.Churro.Prelude

import Control.Concurrent
import Data.Void

instance Transport Chan where
    flex = newChan
    yank = readChan
    yeet = writeChan

type ChurroChan = Churro Chan

-- | Convenience function for running a Churro with a Chan Transport.
-- 
runWaitChan :: ChurroChan Void Void -> IO ()
runWaitChan = runWait

-- | Convenience function for running a Churro into a List with a Chan Transport.
-- 
runWaitListChan :: ChurroChan Void o -> IO [o]
runWaitListChan = runWaitList