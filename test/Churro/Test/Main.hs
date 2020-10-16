
module Main where

import Test.DocTest ( doctest )

-- TODO: Pass additional paths in automatically or via arguments.
main :: IO ()
main = do
    doctest
        [ "-isrc"
        , "-itest"
        , "test/Churro/Test/Examples.hs"
        , "src/Control/Churro/Transport/Unagi/Bounded.hs"
        ]

