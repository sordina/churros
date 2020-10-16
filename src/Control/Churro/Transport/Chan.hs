{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- | Chan Transport Instance.

module Control.Churro.Transport.Chan where
    
import Control.Churro.Types
import Control.Churro.Prelude

import Control.Concurrent
import Data.Void

instance Transport Chan where
    data In  Chan a = ChanIn  (Chan a)
    data Out Chan a = ChanOut (Chan a)
    yank (ChanOut c) = readChan  c
    yeet (ChanIn  c) = writeChan c
    flex = do 
        c <- newChan
        return (ChanIn c, ChanOut c)

type ChurroChan a = Churro a Chan

-- | Convenience function for running a Churro with a Chan Transport.
-- 
runWaitChan :: ChurroChan a Void Void -> IO a
runWaitChan = runWait

-- | Convenience function for running a Churro into a List with a Chan Transport.
-- 
runWaitListChan :: ChurroChan () Void o -> IO [o]
runWaitListChan = runWaitList