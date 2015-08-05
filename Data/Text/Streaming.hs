{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Text.Internal.Lazy
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- A module containing private 'Text' internals. This exposes the
-- 'Text' representation and low level construction functions.
-- Modules which extend the 'Text' system may need to use this module.

module Data.Text.Streaming
    -- (
    --   Text(..)
    -- , chunk
    -- , empty
    -- , foldrChunks
    -- , foldlChunks
    -- -- * Data type invariant and abstraction functions
    --
    -- -- $invariant
    -- , strictInvariant
    -- , lazyInvariant
    -- , showStructure
    --
    -- -- * Chunk allocation sizes
    -- , defaultChunkSize
    -- , smallChunkSize
    -- , chunkOverhead
    -- ) where

import Data.Text ()
import Data.Text.Internal.Unsafe.Shift (shiftL)
import Data.Typeable (Typeable)
import Foreign.Storable (sizeOf)
import qualified Data.Text.Internal as T

data Text m r = 
          Empty r
          | Chunk {-# UNPACK #-} !T.Text (Text m r)
          | Go (m (Text m r))
            deriving (Typeable)

instance Monad m => Functor (Text m) where
  fmap f x = case x of
    Empty a      -> Empty (f a)
    Chunk bs bss -> Chunk bs (fmap f bss)
    Go mbss      -> Go (liftM (fmap f) mbss)

instance Monad m => Applicative (Text m) where
  pure = Empty
  (<*>) = ap

instance Monad m => Monad (Text m) where
  return = Empty
  {-#INLINE return #-}
  x >> y = loop SPEC x where
    loop !_ x = case x of   -- this seems to be insanely effective
      Empty _ -> y
      Chunk a b -> Chunk a (loop SPEC b)
      Go m -> Go (liftM (loop SPEC) m)
  x >>= f =
    -- case x of
    --   Empty a -> f a
    --   Chunk bs bss -> Chunk bs (bss >>= f)
    --   Go mbss      -> Go (liftM (>>= f) mbss)
    loop SPEC x where -- the SPEC seems pointless in simple case
      loop !_ y = case y of
        Empty a -> f a
        Chunk bs bss -> Chunk bs (loop SPEC bss)
        Go mbss      -> Go (liftM (loop SPEC) mbss)

instance MonadIO m => MonadIO (Text m) where
  liftIO io = Go (liftM Empty (liftIO io))
  {-#INLINE liftIO #-}

instance MonadTrans Text where
  lift ma = Go $ liftM Empty ma

data Word8_ r = Word8_ {-#UNPACK#-} !Word8 r

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}
-- -- ------------------------------------------------------------------------
--
-- | Smart constructor for 'Chunk'. Guarantees the data type invariant.
chunk :: S.Text -> Text m r -> Text m r
chunk c@(S.PS _ _ len) cs | len == 0  = cs
                          | otherwise = Chunk c cs
{-# INLINE chunk #-}

yield :: S.Text -> Text m ()
yield bs = chunk bs (Empty ())
{-# INLINE yield #-}


-- | Steptruct a byte stream from its Church encoding (compare @GHC.Exts.build@)
materialize :: (forall x . (r -> x) -> (S.Text -> x -> x) -> (m x -> x) -> x)
            -> Text m r
materialize phi = phi Empty Chunk Go
{-#INLINE materialize #-}

-- | Resolve a byte stream into its Church encoding (compare @Data.List.foldr@)
dematerialize :: Monad m
              => Text m r
              -> (forall x . (r -> x) -> (S.Text -> x -> x) -> (m x -> x) -> x)
dematerialize x nil cons wrap = loop SPEC x
  where
  loop !_ x = case x of
     Empty r    -> nil r
     Chunk b bs -> cons b (loop SPEC bs )
     Go ms -> wrap (liftM (loop SPEC) ms)
{-#INLINE dematerialize #-}

concats :: Monad m => List (Text m) m r -> Text m r
concats x = destroy x Empty join Go

distributed
  :: (Monad m, MonadTrans t, MFunctor t, Monad (t m), Monad (t (Text m)))
  => Text (t m) a
  -> t (Text m) a
distributed ls = dematerialize ls
             return
             (\bs x -> join $ lift $ Chunk bs (Empty x) )
             (join . hoist (Go . fmap Empty))

-- $invariant
--
-- The data type invariant for lazy 'Text': Every 'Text' is either 'Empty' or
-- consists of non-null 'T.Text's.  All functions must preserve this,
-- and the QC properties must check this.

-- | Check the invariant strictly.
-- strictInvariant :: Text -> Bool
-- strictInvariant Empty = True
-- strictInvariant x@(Chunk (T.Text _ _ len) cs)
--     | len > 0   = strictInvariant cs
--     | otherwise = error $ "Data.Text.Lazy: invariant violation: "
--                   ++ showStructure x
--
-- -- | Check the invariant lazily.
-- lazyInvariant :: Text -> Text
-- lazyInvariant Empty = Empty
-- lazyInvariant x@(Chunk c@(T.Text _ _ len) cs)
--     | len > 0   = Chunk c (lazyInvariant cs)
--     | otherwise = error $ "Data.Text.Lazy: invariant violation: "
--                   ++ showStructure x
--
-- -- | Display the internal structure of a lazy 'Text'.
-- showStructure :: Text -> String
-- showStructure Empty           = "Empty"
-- showStructure (Chunk t Empty) = "Chunk " ++ show t ++ " Empty"
-- showStructure (Chunk t ts)    =
--     "Chunk " ++ show t ++ " (" ++ showStructure ts ++ ")"

-- | Smart constructor for 'Chunk'. Guarantees the data type invariant.
chunk :: T.Text -> Text m r -> Text m r
{-# INLINE chunk #-}
chunk t@(T.Text _ _ len) ts | len == 0 = ts
                            | otherwise = Chunk t ts

-- | Smart constructor for 'Empty'.
empty :: Text m ()
{-# INLINE [0] empty #-}
empty = Empty ()

-- -- | Consume the chunks of a lazy ByteString with a natural right fold.
-- foldrChunks :: Monad m => (S.ByteString -> a -> a) -> a -> ByteString m r -> m a
-- foldrChunks step nil bs = dematerialize bs
--   (\_ -> return nil)
--   (liftM . step)
--   join
--
-- {-# INLINE foldrChunks #-}
-- -- | Consume the chunks of a lazy 'Text' with a natural right fold.
-- foldrChunks :: (T.Text -> a -> a) -> a -> Text m r -> m (a, r)
-- foldrChunks f z = go
--   where go (Empty )       = z
--         go (Chunk c cs) = f c (go cs)
-- {-# INLINE foldrChunks #-}
--
-- -- | Consume the chunks of a lazy 'Text' with a strict, tail-recursive,
-- -- accumulating left fold.
-- foldlChunks :: (a -> T.Text -> a) -> a -> Text -> a
-- foldlChunks f z = go z
--   where go !a Empty        = a
--         go !a (Chunk c cs) = go (f a c) cs
-- {-# INLINE foldlChunks #-}
--
-- -- | Currently set to 16 KiB, less the memory management overhead.
-- defaultChunkSize :: Int
-- defaultChunkSize = 16384 - chunkOverhead
-- {-# INLINE defaultChunkSize #-}
--
-- -- | Currently set to 128 bytes, less the memory management overhead.
-- smallChunkSize :: Int
-- smallChunkSize = 128 - chunkOverhead
-- {-# INLINE smallChunkSize #-}
--
-- -- | The memory management overhead. Currently this is tuned for GHC only.
-- chunkOverhead :: Int
-- chunkOverhead = sizeOf (undefined :: Int) `shiftL` 1
-- {-# INLINE chunkOverhead #-}
