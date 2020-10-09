
module Main where

import Test.DocTest

import qualified Churro.Test.Examples as Ex

-- TODO: Find out how to use a different test manager to allow these to be run independently
main :: IO ()
main = do
    doctest ["-isrc", "test/Churro/Test/Examples.hs"]
    Ex.main

