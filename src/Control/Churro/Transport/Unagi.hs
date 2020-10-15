{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}

-- | Chan Transport Instance.

module Control.Churro.Transport.Unagi where
    
import Control.Churro.Types
import Control.Churro.Prelude

import Control.Concurrent.Chan.Unagi
import Data.Void

data Unagi a

instance Transport Unagi where
    data In  Unagi a = ChanIn  (InChan  a)
    data Out Unagi a = ChanOut (OutChan a)
    yank (ChanOut c) = readChan  c
    yeet (ChanIn  c) = writeChan c
    flex = do 
        (i, o) <- newChan
        return (ChanIn i, ChanOut o)

type ChurroUnagi = Churro Unagi

-- | Convenience function for running a Churro with a Chan Transport.
-- 
runWaitChan :: ChurroUnagi Void Void -> IO ()
runWaitChan = runWait

-- | Convenience function for running a Churro into a List with a Chan Transport.
-- 
runWaitListChan :: ChurroUnagi Void o -> IO [o]
runWaitListChan = runWaitList