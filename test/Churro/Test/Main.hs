
module Main where

-- import Test.DocTest

import qualified Churro.Test.Examples as Ex

main :: IO ()
main = do
    Ex.main
    -- doctest ["-isrc", "-itest", "test/Churro/Test/Examples.hs"]

