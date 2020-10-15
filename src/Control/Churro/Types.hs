{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}

-- | Datatypes and definitions used by Churro library.
-- 
-- Expand instances for additional documentation!

module Control.Churro.Types where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Concurrent.Async (cancel, wait, Async, async)
import Data.Void
import Control.Exception (finally)

-- $setup
-- 
-- We import the library for testing, although this would be a circular import in the module itself.
-- 
-- >>> import Control.Churro

-- ** Data, Classes and Instances

-- | The core datatype for the library.
-- 
-- Parameters `t`, `i` and `o` represent the transport, input, and output types respectively.
-- 
-- The items on transports are wrapped in `Maybe` to allow signalling of completion of a source.
-- 
-- When building a program by composing Churros, the output Transport of one
-- Churro is fed into the input Transports of other Churros.
-- 
-- Type families are used to allow the in/out channels to have different types
-- and prevent accidentally reading/writing from the wrong transport.
-- 
-- Convenience types of `Source`, `Sink`, and `DoubleDipped` are also defined,
-- although use is not required.
-- 
data Churro a t i o   = Churro { runChurro :: IO (In t (Maybe i), Out t (Maybe o), Async a) }
type Source a t   o   = Churro a t Void o
type Sink   a t i     = Churro a t i Void
type DoubleDipped a t = Churro a t Void Void

-- | The transport method is abstracted via the Transport class
-- 
-- This allows use of pure or impure channels, such as:
-- 
-- * Chan (Included in `Control.Churro.Transport.Chan`)
-- * TChan
-- * Seq
-- * Unagi
-- * Various buffered options
-- 
-- Transports used in conjunction with Churros wrap items in Maybe so that once
-- a source has been depleted it can signal completion with a Nothing item.
-- 
-- The flex method returns two transports, so that channels such as unagi that
-- create an in/outs pair can have a Transport instance.
-- 
-- Channels like Chan that have a single channel act as in/out simply reuse the
-- same channel in the pair returned.
-- 
class Transport (t :: * -> *) where
    data In  t :: * -> *
    data Out t :: * -> *
    flex :: IO (In t a, Out t a)  -- ^ Create a new pair of transports.
    yank :: Out t a -> IO a       -- ^ Yank an item of the Transport
    yeet :: In t a -> a -> IO ()  -- ^ Yeet an item onto the Transport

-- | Covariant functor instance for Churro - Maps over the output.
-- 
-- >>> let s = sourceList [1,2]
-- >>> runWaitChan $ s >>> sinkPrint
-- 1
-- 2
-- 
-- >>> runWaitChan $ fmap succ s >>> sinkPrint
-- 2
-- 3
instance Transport t => Functor (Churro a t i) where
    fmap f c = Churro do
        (i,o,a) <- runChurro c
        (i',o') <- flex
        a' <- async do
            finally' (cancel a) do
                c2c f o i'
                wait a
        return (i,o',a')

-- | The Category instance allows for the creation of Churro pipelines.
-- 
-- All other examples of the form `a >>> b` use this instance.
-- 
-- The `id` method creates a passthrough arrow.
-- There isn't usually a reason to use `id` directly as it has no effect:
-- 
-- >>> runWaitChan $ pure 1 >>> id >>> id >>> id >>> sinkPrint
-- 1
instance Transport t => Category (Churro () t) where
    id = Churro do
        (i,o) <- flex
        a     <- async (return ())
        return (i,o,a)

    g . f = Churro do
        (fi, fo, fa) <- runChurro f
        (gi, go, ga) <- runChurro g
        a <- async do c2c id fo gi
        b <- async do
            finally' (cancel a >> cancel fa >> cancel ga) do
                wait ga
                cancel fa
                cancel a
        return (fi, go, b)

-- | The Applicative instance allows for pairwise composition of Churro pipelines.
--   Once again this is covariat and the composition occurs on the output transports of the Churros.
-- 
--  The `pure` method allows for the creation of a Churro yielding a single item.
-- 
instance Transport t => Applicative (Churro () t Void) where
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

        -- TODO: Should we cancel asyncs here in finally block?
        prog
        yeet o Nothing
        wait fa
        wait ga

-- | The Arrow instance allows for building non-cyclic directed graphs of churros.
-- 
--  The `arr` method allows for the creation of a that maps items with a pure function.
--  This is equivalent to `fmap f id`.
-- 
-- >>> :set -XArrows
-- >>> :{
-- let sect  = process $ \x@(_x,_y,z) -> print x >> return z
--     graph =
--       proc i -> do
--         j <- arr succ  -< i
--         k <- arr show  -< j
--         l <- arr succ  -< j
--         m <- arr (> 5) -< j
--         n <- sect      -< (k,l,m)
--         o <- arr not   -< n
--         p <- delay 0.1 -< o
--         sinkPrint      -< p
-- in
-- runWaitChan $ sourceList [1,5,30] >>> graph
-- :}
-- ("2",3,False)
-- ("6",7,True)
-- ("31",32,True)
-- True
-- False
-- False
-- 
-- The other Arrow methods are also usable:
-- 
-- >>> runWaitChan $ pure 1 >>> (arr show &&& arr succ) >>> sinkPrint
-- ("1",2)
instance Transport t => Arrow (Churro () t) where
    arr = flip fmap id

    first c = Churro do
        (i,o,a)   <- runChurro c
        (ai',ao') <- flex
        (bi',bo') <- flex

        let go = do
                is <- yank ao'
                yeet i (fmap fst is)

                os <- yank o
                yeet bi' $ (,) <$> os <*> fmap snd is

                case (is, os) of
                    (Just _, Just _) -> go
                    _ -> return ()

        a' <- async do
            go
            yeet bi' Nothing
            wait a

        return (ai',bo',a')

-- ** Helpers

-- | A helper to facilitate constructing a Churro that makes new input and output transports available for manipulation.
-- 
-- The manipulations performed are carried out in the async action associated with the Churro
-- 
buildChurro :: Transport t => (Out t (Maybe i) -> In t (Maybe o) -> IO a) -> Churro a t i o
buildChurro cb = Churro do
    (ai,ao) <- flex
    (bi,bo) <- flex
    a       <- async do cb ao bi
    return (ai,bo,a)

-- | A version of `buildChurro` that also passes the original input to the callback so that you can reschedule items.
-- 
-- Used by "retry" style functions.
-- 
buildChurro' :: Transport t => (In t (Maybe i) -> Out t (Maybe i) -> In t (Maybe o) -> IO a) -> Churro a t i o
buildChurro' cb = Churro do
    (ai,ao) <- flex
    (bi,bo) <- flex
    a       <- async do cb ai ao bi
    return (ai,bo,a)

-- | Yeet all items from a list into a transport.
-- 
yeetList :: (Foldable f, Transport t) => In t a -> f a -> IO ()
yeetList t = mapM_ (yeet t)

-- | Yank all items from a Raw transport into a list.
-- 
--   Won't terminate until the transport has been consumed.
-- 
yankList :: Transport t => Out t (Maybe a) -> IO [a]
yankList t = do
    x <- yank t
    case x of 
        Nothing -> return []
        Just y  -> (y :) <$> yankList t

-- | Yank each item from a transport into a callback.
-- 
yankAll :: Transport t => Out t (Maybe i) -> (i -> IO a) -> IO ()
yankAll c f = do
    x <- yank c
    case x of
        Nothing -> return ()
        Just y  -> f y >> yankAll c f

-- | Yank each raw item from a transport into a callback.
-- 
-- The items are wrapped in Maybes and when all items are yanked, Nothing is fed to the callback.
-- 
yankAll' :: Transport t => Out t (Maybe a) -> (Maybe a -> IO b) -> IO b
yankAll' c f = do
    yankAll c (f . Just)
    f Nothing

-- | Yank then Yeet each item from one Transport into another.
-- 
-- Raw items are used so `Nothing` should be Yeeted once the transport is depleted.
-- 
c2c :: Transport t => (a -> b) -> Out t (Maybe a) -> In t (Maybe b) -> IO ()
c2c f o i = yankAll' o (yeet i . fmap f)

-- | Flipped `finally`.
finally' :: IO b -> IO a -> IO a
finally' = flip finally