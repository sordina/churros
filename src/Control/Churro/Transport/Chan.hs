{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

-- | Datatypes and definitions used by Churro library

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

runWaitChan :: ChurroChan Void Void -> IO ()
runWaitChan = runWait