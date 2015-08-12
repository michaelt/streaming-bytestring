{-# LANGUAGE CPP, BangPatterns #-}
{-#LANGUAGE RankNTypes, GADTs #-}
module Data.ByteString.Streaming.Internal (
   ByteString (..) 
   , Word8_ (..)
   , chunk             -- :: S.ByteString -> ByteString m r -> ByteString m r
   , chunkOverhead     -- :: Int
   , defaultChunkSize  -- :: Int
   , materialize       -- :: (forall x. (r -> x) -> (ByteString -> x -> x) -> (m x -> x) -> x) -> ByteString m r
   , dematerialize     -- :: Monad m =>  ByteString m r -> forall x.  (r -> x) -> (ByteString -> x -> x) -> (m x -> x) -> x
   , foldrChunks       -- :: Monad m =>  (ByteString -> a -> a) -> a -> ByteString m r -> m a
   , foldrChunksM       -- :: Monad m => (ByteString -> m a -> m a) -> m a -> ByteString m r -> m a
   , packBytes          -- :: Monad m => [GHC.Word.Word8] -> ByteString m ()
   , smallChunkSize     -- :: Int
   , unpackAppendBytesLazy  -- :: ByteString -> Stream Word8_ m r  -> Stream Word8_ m r
   , unpackAppendBytesStrict  -- :: ByteString -> Stream Word8_ m r  -> Stream Word8_ m r
   , unpackBytes        -- :: Monad m => ByteString m r -> Stream Word8_ m r
   , yield              --  :: ByteString -> ByteString m ()
  ) where

import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)
import qualified Prelude
import qualified Data.List              as L  -- L for list/lazy
import qualified Data.ByteString.Lazy.Internal as BI  -- just for fromChunks etc
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad

import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S

import Streaming.Internal hiding (yield, uncons, concat, concats, append, materialize, dematerialize)
import qualified Streaming.Internal as Type
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts ( SpecConstrAnnotation(..) )
import Data.String
import Data.Functor.Identity
import Data.Word

-- | A space-efficient representation of a succession of 'Word8' vectors, supporting many
-- efficient operations.
--
-- An effectful 'ByteString' contains 8-bit bytes, or by using the operations
-- from "Data.ByteString.Streaming.Char8" it can be interpreted as containing
-- 8-bit characters.

data ByteString m a =
  Empty a
  | Chunk {-#UNPACK #-} !S.ByteString (ByteString m a )
  | Go (m (ByteString m a ))

instance Monad m => Functor (ByteString m) where
  fmap f x = case x of
    Empty a      -> Empty (f a)
    Chunk bs bss -> Chunk bs (fmap f bss)
    Go mbss      -> Go (liftM (fmap f) mbss)

instance Monad m => Applicative (ByteString m) where
  pure = Empty
  (<*>) = ap

instance Monad m => Monad (ByteString m) where
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
    loop SPEC x where -- unlike >> this SPEC seems pointless 
      loop !_ y = case y of
        Empty a -> f a
        Chunk bs bss -> Chunk bs (loop SPEC bss)
        Go mbss      -> Go (liftM (loop SPEC) mbss)

instance MonadIO m => MonadIO (ByteString m) where
  liftIO io = Go (liftM Empty (liftIO io))
  {-#INLINE liftIO #-}

instance MonadTrans ByteString where
  lift ma = Go $ liftM Empty ma


instance (r ~ ()) => IsString (ByteString m r) where
  fromString = yield . S.pack . Prelude.map S.c2w

instance (m ~ Identity, Show r) => Show (ByteString m r) where
  show bs = case bs of
    Empty r -> "Empty (" ++ show r ++ ")"
    Go (Identity bs') -> "Go (Identity (" ++ show bs' ++ "))"
    Chunk bs'' bs -> "Chunk " ++ show bs'' ++ " (" ++ show bs ++ ")"
    
instance (Monoid r, Monad m) => Monoid (ByteString m r) where
  mempty = Empty mempty
  mappend = liftM2 mappend
      
data Word8_ r = Word8_ {-#UNPACK#-} !Word8 r

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}
-- -- ------------------------------------------------------------------------
--
-- | Smart constructor for 'Chunk'.
chunk :: S.ByteString -> ByteString m r -> ByteString m r
chunk c@(S.PS _ _ len) cs | len == 0  = cs
                          | otherwise = Chunk c cs
{-# INLINE chunk #-}

-- | Yield-style mart constructor for 'Chunk'.
yield :: S.ByteString -> ByteString m ()
yield bs = chunk bs (Empty ())
{-# INLINE yield #-}


-- | Construct a byte stream from its Church encoding (compare @GHC.Exts.build@)
materialize :: (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)
            -> ByteString m r
materialize phi = phi Empty Chunk Go
{-#INLINE materialize #-}

-- | Resolve a byte stream into its Church encoding (compare @Data.Stream.foldr@)
dematerialize :: Monad m
              => ByteString m r
              -> (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)
dematerialize x nil cons wrap = loop SPEC x
  where
  loop !_ x = case x of
     Empty r    -> nil r
     Chunk b bs -> cons b (loop SPEC bs )
     Go ms -> wrap (liftM (loop SPEC) ms)
{-#INLINE dematerialize #-}
------------------------------------------------------------------------

-- The representation uses lists of packed chunks. When we have to convert from
-- a lazy list to the chunked representation, then by default we use this
-- chunk size. Some functions give you more control over the chunk size.
--
-- Measurements here:
--  http://www.cse.unsw.edu.au/~dons/tmp/chunksize_v_cache.png
--
-- indicate that a value around 0.5 to 1 x your L2 cache is best.
-- The following value assumes people have something greater than 128k,
-- and need to share the cache with other programs.

-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)

-- ------------------------------------------------------------------------
-- | Packing and unpacking from lists
packBytes :: Monad m => [Word8] -> ByteString m ()
packBytes cs0 =
    packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenBytes n cs of
      (bs, [])  -> Chunk bs (Empty ())
      (bs, cs') -> Chunk bs (packChunks (min (n * 2) BI.smallChunkSize) cs')

unpackAppendBytesLazy :: S.ByteString -> Stream Word8_ m r -> Stream Word8_ m r
unpackAppendBytesLazy (S.PS fp off len) xs
  | len <= 100 = unpackAppendBytesStrict (S.PS fp off len) xs
  | otherwise  = unpackAppendBytesStrict (S.PS fp off 100) remainder
  where
    remainder  = unpackAppendBytesLazy (S.PS fp (off+100) (len-100)) xs

unpackAppendBytesStrict :: S.ByteString -> Stream Word8_ m r -> Stream Word8_ m r
unpackAppendBytesStrict (S.PS fp off len) xs =
 S.accursedUnutterablePerformIO $ withForeignPtr fp $ \base -> do
      loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (Step (Word8_ x acc))

unpackBytes :: Monad m => ByteString m r ->  Stream Word8_ m r
unpackBytes bss = dematerialize bss
  Return
  unpackAppendBytesLazy
  Delay

-- | Consume the chunks of an effectful ByteString with a natural right fold.
foldrChunks :: Monad m => (S.ByteString -> a -> a) -> a -> ByteString m r -> m a
foldrChunks step nil bs = dematerialize bs
  (\_ -> return nil)
  (liftM . step)
  join
{-# INLINE foldrChunks #-}

-- | Consume the chunks of an effectful ByteString with a natural right monadic fold.
foldrChunksM :: Monad m => (S.ByteString -> m a -> m a) -> m a -> ByteString m r -> m a
foldrChunksM step nil bs = dematerialize bs
  (\_ -> nil)
  step
  join
{-# INLINE foldrChunksM #-}
