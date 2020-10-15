
module Main where

import Test.DocTest

-- import qualified Churro.Test.Examples as Ex
-- import qualified Build_doctests       as DT

-- TODO: Find out how to use a different test manager to allow these to be run independently
main :: IO ()
main = do
    doctest
        [ "-isrc"
        , "-itest"
        , "test/Churro/Test/Examples.hs"
        , "src/Control/Churro/Transport/Unagi/Bounded.hs"
        ]

