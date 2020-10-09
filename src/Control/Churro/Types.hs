{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}

-- | Datatypes and definitions used by Churro library

module Control.Churro.Types where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Concurrent.Async (cancel, wait, Async, async)
import Data.Void

-- Data, Classes and Instances

data Churro t i o = Churro { runChurro :: IO (t (Maybe i), t (Maybe o), Async ()) }

class Transport t where
    flex     :: IO (t a)
    yank     :: t a -> IO a
    yeet     :: t a -> a -> IO ()

instance Transport t => Functor (Churro t a) where
    fmap f c = Churro do
        (i,o,a) <- runChurro c
        o'  <- flex
        a'  <- async do
            c2c f o o'
            wait a
        return (i,o',a')

instance Transport t => Category (Churro t) where
    id    = Churro do async (return ()) >>= \a -> flex >>= \c -> return (c,c,a)
    g . f = Churro do
        (fi, fo, fa) <- runChurro f
        (gi, go, ga) <- runChurro g
        a <- async do c2c id fo gi
        b <- async do
            wait ga
            cancel fa
            wait a
        return (fi, go, b)

instance Transport t => Applicative (Churro t Void) where
    pure x = buildChurro \_i o -> yeet o (Just x) >> yeet o Nothing

    f <*> g = buildChurro \_i o -> do
        (_fi, fo, fa) <- runChurro f
        (_gi, go, ga) <- runChurro g

        let
            prog :: IO ()
            prog = do
                fx <- yank fo
                gx <- yank go
                case (fx, gx) of
                    (Just f', Just g') -> (yeet o $ Just (f' g')) >> prog
                    _                  -> return ()

        prog
        yeet o Nothing
        wait fa
        wait ga

instance Transport t => Arrow (Churro t) where
    arr = flip fmap id

    first c = Churro do
        (i,o,a) <- runChurro c
        i'      <- flex
        o'      <- flex

        let go = do
                is <- yank i'
                yeet i (fmap fst is)

                os <- yank o
                yeet o' $ (,) <$> os <*> fmap snd is

                case (is, os) of
                    (Just _, Just _) -> go
                    _ -> return ()

        a' <- async do
            go
            yeet o' Nothing
            wait a

        return (i',o',a')

buildChurro :: Transport t => (t (Maybe i) -> t (Maybe o) -> IO ()) -> Churro t i o
buildChurro cb = Churro do
    i <- flex
    o <- flex
    a <- async do cb i o
    return (i,o,a)

yeetList :: (Foldable t1, Transport t2) => t2 a -> t1 a -> IO ()
yeetList t = mapM_ (yeet t)

yankAll :: Transport t => t (Maybe i) -> (i -> IO a) -> IO ()
yankAll c f = do
    x <- yank c
    case x of
        Nothing -> return ()
        Just y  -> f y >> yankAll c f

yankAll' :: Transport t => t (Maybe a) -> (Maybe a -> IO b) -> IO b
yankAll' c f = do
    yankAll c (f . Just)
    f Nothing

c2c :: Transport t => (a1 -> a2) -> t (Maybe a1) -> t (Maybe a2) -> IO ()
c2c f i o = yankAll' i (yeet o . fmap f)
