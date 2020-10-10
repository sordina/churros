{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}

-- | Datatypes and definitions used by Churro library.

module Control.Churro.Types where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Concurrent.Async (cancel, wait, Async, async)
import Data.Void

-- ** Data, Classes and Instances

-- | The core datatype for the library.
--   Parameters`t`, `i` and `o` represent the transport,
--   input and output types respectively.
-- 
-- When building a program by composing Churros, the output Transport of one Churro is fed into the input Transports of other Churros.
-- 
data Churro t i o = Churro { runChurro :: IO (t (Maybe i), t (Maybe o), Async ()) }

-- | The transport method is abstracted via the Transport class
-- 
-- This allows use of pure or impure channels, such as:
-- 
-- * Chan (Included in `Control.Churro.Transport.Chan`)
-- * TChan
-- * Seq
-- * Various buffered options
-- 
-- Transports used in conjunction with Churros wrap items in Maybe so that once a source has been depleted it can signal completion with
-- a Nothing item.
-- 
class Transport t where
    flex :: IO (t a)           -- ^ Create a new Transport
    yank :: t a -> IO a        -- ^ Yank an item of the Transport
    yeet :: t a -> a -> IO ()  -- ^ Yeet an item onto the Transport

-- | Covariant functor instance for Churro - Maps over the output.
instance Transport t => Functor (Churro t a) where
    fmap f c = Churro do
        (i,o,a) <- runChurro c
        o'  <- flex
        a'  <- async do
            c2c f o o'
            wait a
        return (i,o',a')

-- | The Category instance allows for the creation of Churro pipelines.
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

-- | The Applicative instance allows for pairwise composition of Churro pipelines.
--   Once again this is covariat and the composition occurs on the output transports of the Churros.
-- 
--  The `pure` method allows for the creation of a Churro yielding a single item.
-- 
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

-- | The Arrow instance allows for building non-cyclic directed graphs of churros.
-- 
--  The `arr` method allows for the creation of a that maps items with a pure function.
--  This is equivalent to `fmap f id`.
-- 
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

-- ** Helpers

-- | A helper to facilitate constructing a Churro that makes new input and output transports available for manipulation.
-- 
-- The manipulations performed are carried out in the async action associated with the Churro
-- 
buildChurro :: Transport t => (t (Maybe i) -> t (Maybe o) -> IO ()) -> Churro t i o
buildChurro cb = Churro do
    i <- flex
    o <- flex
    a <- async do cb i o
    return (i,o,a)

-- | Yeet all items from a list into a transport.
-- 
yeetList :: (Foldable t1, Transport t2) => t2 a -> t1 a -> IO ()
yeetList t = mapM_ (yeet t)

-- | Yank each item from a transport into a callback.
-- 
yankAll :: Transport t => t (Maybe i) -> (i -> IO a) -> IO ()
yankAll c f = do
    x <- yank c
    case x of
        Nothing -> return ()
        Just y  -> f y >> yankAll c f

-- | Yank each raw item from a transport into a callback.
-- 
-- The items are wrapped in Maybes and when all items are yanked, Nothing is fed to the callback.
-- 
yankAll' :: Transport t => t (Maybe a) -> (Maybe a -> IO b) -> IO b
yankAll' c f = do
    yankAll c (f . Just)
    f Nothing

-- | Yank then Yeet each item from one Transport into another.
-- 
-- Raw items are used so `Nothing` should be Yeeted once the transport is depleted.
-- 
c2c :: Transport t => (a1 -> a2) -> t (Maybe a1) -> t (Maybe a2) -> IO ()
c2c f i o = yankAll' i (yeet o . fmap f)
