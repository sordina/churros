{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Churro.FGL where

import Data.Graph.Inductive.PatriciaTree
-- import Data.Graph.Inductive.Graph

type ChurroGraph = Gr () Name
type EdgeInfo    = (Name, Type, Process, Args)
type Name        = String
type Type        = String
type Process     = String
type Args        = [String]
