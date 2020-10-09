{-# LANGUAGE BlockArguments #-}

-- | Datatypes and definitions used by Churro library

module Control.Churro.Transport

    ( module Control.Churro.Transport.Chan
    , module Control.Churro.Transport
    )

where

import Control.Churro.Transport.Chan
import Control.Churro.Types

l2c :: Transport t => t (Maybe a) -> [a] -> IO ()
l2c c l = mapM_ (yeet c . Just) l >> yeet c Nothing