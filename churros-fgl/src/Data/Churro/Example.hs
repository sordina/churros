{-# LANGUAGE TypeApplications #-}

module Data.Churro.Example where

import Control.Churro
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Graph

main :: IO ()
main = do
    g <- loadGraph "test/graph.dot"
    runWaitChan g

readGraph :: String -> IO (DotGraph Int)
readGraph f = readDotFile f

loadGraph :: (Transport t ) => String -> IO (Churro () t Void Void)
loadGraph f = do
    r <- readGraph f
    print r
    return undefined
