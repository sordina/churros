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

type ChurroUnagi a = Churro a Unagi

-- | Convenience function for running a Churro with an Unagi Transport.
-- 
runWaitUnagi :: ChurroUnagi a Void Void -> IO a
runWaitUnagi = runWait

-- | Convenience function for running a Churro into a List with an Unagi Transport.
-- 
runWaitListUnagi :: ChurroUnagi () Void o -> IO [o]
runWaitListUnagi = runWaitList