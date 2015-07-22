{-# LANGUAGE CPP, BangPatterns #-}
{-#LANGUAGE RankNTypes #-}
-- This library emulates Data.ByteString.Lazy but includes a little 'FreeT' library

-- |
-- Module      : Data.ByteString.Lazy
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time and space-efficient implementation of lazy byte vectors
-- using lists of packed 'Word8' arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Lazy ByteStrings are encoded as lazy lists of strict chunks
-- of bytes.
--
-- A key feature of lazy ByteStrings is the means to manipulate large or
-- unbounded streams of data without requiring the entire sequence to be
-- resident in memory. To take advantage of this you have to write your
-- functions in a lazy streaming style, e.g. classic pipeline composition. The
-- default I\/O chunk size is 32k, which should be good in most circumstances.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons', have
-- better complexity than their "Data.ByteString" equivalents, due to
-- optimisations resulting from the list spine structure. For other
-- operations lazy ByteStrings are usually within a few percent of
-- strict ones.
--
-- The recomended way to assemble lazy ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'Foreign.ForeignPtr.ForeignPtr'
-- by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
-- Lazy variant by Duncan Coutts and Don Stewart.
--
--
module Data.ByteString.Streaming (
  ByteString (..),
  filter,
  cycle,
  iterate,
  repeat,
  singleton,
  pack,
  empty,
  unfoldr,
  yield,
  map,
  maps,
  span,
  split,
  splitAt,
  splitWith,
  take,
  drop,
  takeWhile,
  toChunks,
  break,
  fromChunks,
  append,
  concat,
  concats,
  cons,
  stdin,
  stdout,
  getContents,
  hGet,
  hGetContents,
  fromHandle,
  hGetContentsN,
  hGetN,
  hGetNonBlocking,
  hGetNonBlockingN,
  hPut,
  toHandle,
  readFile,
  writeFile,
  appendFile,
  uncons,
  head,
  intercalate,
  intersperse,
  group,
  null,
  zipWithList,
  chunk,
  distributed,
  fromStrict,
  toStrict,
  hPutNonBlocking,
  interact  
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


import qualified Data.ByteString        as P  (ByteString) -- type name only
import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S

import Data.ByteString.Streaming.Internal.Type hiding (uncons, concat, append, materialize, dematerialize)
import qualified Data.ByteString.Streaming.Internal.Type as Type

import Data.Monoid         

import Control.Monad            (mplus,liftM, join, ap)
import Control.Monad.Trans      
import Control.Monad.Morph

import Data.Word                (Word8)
import Data.Int                 (Int64)
import System.IO                (Handle,openBinaryFile,IOMode(..)
                                ,hClose)
import qualified System.IO as IO (stdin, stdout)
import System.IO.Error          (mkIOError, illegalOperationErrorType)
import System.IO.Unsafe
import Control.Exception        (bracket)

import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts ( SpecConstrAnnotation(..) )


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
    loop SPEC x where -- the SPEC seems pointless in simple case
      loop !_ y = case y of
        Empty a -> f a
        Chunk bs bss -> Chunk bs (loop SPEC bss)
        Go mbss      -> Go (liftM (loop SPEC) mbss)

instance MonadIO m => MonadIO (ByteString m) where
  liftIO io = Go (liftM Empty (liftIO io))
  {-#INLINE liftIO #-}

instance MonadTrans ByteString where
  lift ma = Go $ liftM Empty ma
  
data Word8_ r = Word8_ {-#UNPACK#-} !Word8 r

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}
-- -- ------------------------------------------------------------------------
--
-- | Smart constructor for 'Chunk'. Guarantees the data type invariant.
chunk :: S.ByteString -> ByteString m r -> ByteString m r
chunk c@(S.PS _ _ len) cs | len == 0  = cs
                          | otherwise = Chunk c cs
{-# INLINE chunk #-}

yield :: S.ByteString -> ByteString m () 
yield c@(S.PS _ _ len) | len == 0  = Empty ()
                       | otherwise = Chunk c (Empty ())
{-#INLINE yield #-}


-- | Steptruct a byte stream from its Church encoding (compare @GHC.Exts.build@)
materialize :: (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)
            -> ByteString m r
materialize phi = phi Empty Chunk Go
{-#INLINE materialize #-}

-- | Resolve a byte stream into its Church encoding (compare @Data.List.foldr@)
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

concats :: Monad m => List (ByteString m) m r -> ByteString m r
concats x = destroy x Empty join Go

distributed
  :: (Monad m, MonadTrans t, MFunctor t, Monad (t m), Monad (t (ByteString m))) 
  => ByteString (t m) a 
  -> t (ByteString m) a
distributed ls = dematerialize ls 
             return 
             (\bs x -> join $ lift $ Chunk bs (Empty x) )
             (join . hoist (Go . fmap Empty))

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
-- Packing and unpacking from lists
packBytes :: Monad m => [Word8] -> ByteString m ()
packBytes cs0 =
    packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenBytes n cs of
      (bs, [])  -> Chunk bs (Empty ())
      (bs, cs') -> Chunk bs (packChunks (min (n * 2) BI.smallChunkSize) cs')

unpackAppendBytesLazy :: S.ByteString -> List Word8_ m r -> List Word8_ m r
unpackAppendBytesLazy (S.PS fp off len) xs
  | len <= 100 = unpackAppendBytesStrict (S.PS fp off len) xs
  | otherwise  = unpackAppendBytesStrict (S.PS fp off 100) remainder
  where
    remainder  = unpackAppendBytesLazy (S.PS fp (off+100) (len-100)) xs

unpackAppendBytesStrict :: S.ByteString -> List Word8_ m r -> List Word8_ m r
unpackAppendBytesStrict (S.PS fp off len) xs =
 S.accursedUnutterablePerformIO $ withForeignPtr fp $ \base -> do
      loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (Step (Word8_ x acc))

unpackBytes :: Monad m => ByteString m r ->  List Word8_ m r
unpackBytes bss = dematerialize bss 
  Return
  unpackAppendBytesLazy
  Wrap

-- unpackBytes (Empty r)   = Return r
-- unpackBytes (Chunk c cs) = unpackAppendBytesLazy c (unpackBytes cs)
-- unpackBytes (Go m)       = Wrap (liftM unpackBytes m)


-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString m ()
empty = Empty ()
{-# INLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Monad m => Word8 -> ByteString m ()
singleton w = Chunk (S.singleton w)  (Empty ())
{-# INLINE singleton #-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'.
pack :: Monad m => [Word8] -> ByteString m ()
pack = packBytes
{-#INLINE pack #-}

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
-- unpack :: ByteString -> [Word8]
-- unpack = unpackBytes

-- | /O(c)/ Convert a list of strict 'ByteString' into a lazy 'ByteString'
fromChunks :: Monad m => [P.ByteString] -> ByteString m ()
fromChunks cs = L.foldr chunk (Empty ()) cs

-- -- | /O(c)/ Convert a lazy 'ByteString' into a list of strict 'ByteString'
toChunks :: Monad m => ByteString m () -> m [P.ByteString]
toChunks bs = 
  dematerialize bs 
      (\() -> return [])
      (\b mx -> liftM (b:) mx)
      join
  -- type ByteString_ m r = 
    -- (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)

  -- foldrChunks (\a bs -> liftM (a :) bs) (return [])

-- |/O(1)/ Convert a strict 'ByteString' into a lazy 'ByteString'.
fromStrict :: P.ByteString -> ByteString m ()
fromStrict bs | S.null bs = Empty ()
              | otherwise = Chunk bs  (Empty ())

-- |/O(n)/ Convert a lazy 'ByteString' into a strict 'ByteString'.
--
-- Note that this is an /expensive/ operation that forces the whole lazy
-- ByteString into memory and then copies all the data. If possible, try to
-- avoid converting back and forth between strict and lazy bytestrings.

toStrict :: Monad m => ByteString m () -> m (S.ByteString)
toStrict bs = dematerialize bs 
  (\r -> return S.empty)
  (\bs mbs -> liftM (S.append bs) mbs)
  join
  
toStrict' cs0 = 
  do bss <- toChunks cs0
     let totalLen = (S.checkedSum "Lazy.toStrict" . L.map S.length) bss
     return $ S.unsafeCreate totalLen $ \ptr -> go bss ptr
  where
    go []                      !_       = return ()
    go (S.PS fp off len : cs) !destptr =
      withForeignPtr fp $ \p -> do
        S.memcpy destptr (p `plusPtr` off) len
        go cs (destptr `plusPtr` len)

toStrict'' :: MonadIO m => ByteString m () -> m (S.ByteString)
toStrict'' cs0 = 
  do bss <- toChunks cs0
     let totalLen = (S.checkedSum "Lazy.toStrict" . L.map S.length) bss
     liftIO $ S.create totalLen $ \ptr -> go bss ptr
  where
    go []                      !_       = return ()
    go (S.PS fp off len : cs) !destptr =
      withForeignPtr fp $ \p -> do
        S.memcpy destptr (p `plusPtr` off) len
        go cs (destptr `plusPtr` len)
        
toLazy :: Monad m => ByteString m () -> m BI.ByteString    
toLazy bs = dematerialize bs 
                (\() -> return (BI.Empty))
                (\b mx -> liftM (BI.Chunk b) mx)
                join
                                   
fromLazy :: Monad m => BI.ByteString -> ByteString m ()
fromLazy = BI.foldrChunks Chunk (Empty ()) 



-- | Consume the chunks of a lazy ByteString with a natural right fold.
foldrChunks :: Monad m => (S.ByteString -> a -> a) -> a -> ByteString m r -> m a
foldrChunks step nil bs = dematerialize bs
  (\_ -> return nil)
  (liftM . step)
  join

{-# INLINE foldrChunks #-}


foldrChunksM :: Monad m => (S.ByteString -> m a -> m a) -> m a -> ByteString m r -> m a
foldrChunksM step nil bs = dematerialize bs
  (\_ -> nil)
  step
  join

{-# INLINE foldrChunksM #-}
--  (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)

-- ------------------------------------------------------------------------
-- --
-- -- {-
-- -- -- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- -- -- conversion function
-- -- packWith :: (a -> Word8) -> [a] -> ByteString
-- -- packWith k str = LPS $ L.map (P.packWith k) (chunk defaultChunkSize str)
-- -- {-# INLINE packWith #-}
-- -- {-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}
-- --
-- -- -- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
-- -- unpackWith :: (Word8 -> a) -> ByteString -> [a]
-- -- unpackWith k (LPS ss) = L.concatMap (S.unpackWith k) ss
-- -- {-# INLINE unpackWith #-}
-- -- {-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}
-- -- -}
-- --
-- -- -- ---------------------------------------------------------------------
-- -- -- Basic interface
-- --
-- | /O(1)/ Test whether a ByteString is empty.
null :: Monad m => ByteString m r -> m Bool
null (Empty _)  = return True
null (Go m)     = m >>= null
null _          = return False
{-# INLINE null #-}

-- | /O(n\/c)/ 'length' returns the length of a ByteString as an 'Int64'
-- length :: Monad m => ByteString m r -> m Int64
-- length cs = foldlChunks (\n c -> n + fromIntegral (S.length c)) 0 cs
-- {-# INLINE length #-}

-- infixr 5 `cons` -- , `cons'` --same as list (:)
-- -- nfixl 5 `snoc`
--
-- | /O(1)/ 'cons' is analogous to '(:)' for lists.
--
cons :: Monad m => Word8 -> ByteString m r -> ByteString m r
cons c cs = Chunk (S.singleton c) cs
{-# INLINE cons #-}

-- | /O(1)/ Unlike 'cons', 'cons\'' is
-- strict in the ByteString that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons\' c xs in xs
--
-- You can however use 'cons', as well as 'repeat' and 'cycle', to build
-- infinite lazy ByteStrings.
--
cons' :: Word8 -> ByteString m r -> ByteString m r
cons' w (Chunk c cs) | S.length c < 16 = Chunk (S.cons w c) cs
cons' w cs                             = Chunk (S.singleton w) cs
{-# INLINE cons' #-}
-- --
-- -- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
-- snoc :: ByteString -> Word8 -> ByteString
-- snoc cs w = foldrChunks Chunk (singleton w) cs
-- {-# INLINE snoc #-}
--
-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: Monad m => ByteString m r -> m Word8
head (Empty _)   = error "head"
head (Chunk c _) = return $ S.unsafeHead c
head (Go m)      = m >>= head
{-# INLINE head #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: Monad m => ByteString m r -> m (Either r (Word8, ByteString m r))
uncons (Empty r) = return (Left r)
uncons (Chunk c cs)
    = return $ Right (S.unsafeHead c
                     , if S.length c == 1 
                         then cs 
                         else Chunk (S.unsafeTail c) cs )
uncons (Go m) = m >>= uncons
{-# INLINE uncons #-}

--
--
-- breakSubstring :: S.ByteString -- ^ String to search for
--                -> ByteString m r-- ^ String to search in
--                -> ByteString m (ByteString m r) -- ^ Head and tail of string broken at substring
--
-- breakSubstring pat src = search 0 src
--   where
--     search !n !s = case s of
--      Empty r -> Empty (Empty r)
   --   Chunk c 
        -- | null s             = (src,empty)      -- not found
        -- | pat `isPrefixOf` s = (take n src,s)
        -- | otherwise          = search (n+1) (unsafeTail s)
-- -- | /O(1)/ Extract the elements after the head of a ByteString, which must be
-- -- non-empty.
-- tail :: ByteString -> ByteString
-- tail Empty          = errorEmptyList "tail"
-- tail (Chunk c cs)
--   | S.length c == 1 = cs
--   | otherwise       = Chunk (S.unsafeTail c) cs
-- {-# INLINE tail #-}
--
-- -- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite
-- -- and non-empty.
-- last :: ByteString -> Word8
-- last Empty          = errorEmptyList "last"
-- last (Chunk c0 cs0) = go c0 cs0
--   where go c Empty        = S.unsafeLast c
--         go _ (Chunk c cs) = go c cs
-- -- XXX Don't inline this. Something breaks with 6.8.2 (haven't investigated yet)
--
-- -- | /O(n\/c)/ Return all the elements of a 'ByteString' except the last one.
-- init :: ByteString -> ByteString
-- init Empty          = errorEmptyList "init"
-- init (Chunk c0 cs0) = go c0 cs0
--   where go c Empty | S.length c == 1 = Empty
--                    | otherwise       = Chunk (S.unsafeInit c) Empty
--         go c (Chunk c' cs)           = Chunk c (go c' cs)
--
-- -- | /O(n\/c)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- -- if it is empty.
-- --
-- -- * It is no faster than using 'init' and 'last'
-- unsnoc :: ByteString -> Maybe (ByteString, Word8)
-- unsnoc Empty        = Nothing
-- unsnoc (Chunk c cs) = Just (init (Chunk c cs), last (Chunk c cs))

-- | /O(n\/c)/ Append two 
append :: Monad m => ByteString m r -> ByteString m s -> ByteString m s
append xs ys = dematerialize xs (const ys) Chunk Go
{-# INLINE append #-}
--
-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: Monad m => (Word8 -> Word8) -> ByteString m r -> ByteString m r
map f z = dematerialize z
   Empty
   (\bs x -> Chunk (S.map f bs) x)
   Go
-- map f s = go s
--     where
--         go (Empty r)    = Empty r
--         go (Chunk x xs) = Chunk y ys
--             where
--                 y  = S.map f x
--                 ys = go xs
--         go (Go mbs) = Go (liftM go mbs)
{-# INLINE map #-}
--
-- -- | /O(n)/ 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- reverse :: ByteString -> ByteString
-- reverse cs0 = rev Empty cs0
--   where rev a Empty        = a
--         rev a (Chunk c cs) = rev (Chunk (S.reverse c) a) cs
-- {-# INLINE reverse #-}
--
-- -- | The 'intersperse' function takes a 'Word8' and a 'ByteString' and
-- -- \`intersperses\' that byte between the elements of the 'ByteString'.
-- -- It is analogous to the intersperse function on Lists.
intersperse :: Monad m => Word8 -> ByteString m r -> ByteString m r
intersperse _ (Empty r)    = Empty r
intersperse w (Chunk c cs) = Chunk (S.intersperse w c)
                                   (dematerialize cs Empty (Chunk . intersperse') Go)
  where intersperse' :: P.ByteString -> P.ByteString
        intersperse' (S.PS fp o l) =
          S.unsafeCreate (2*l) $ \p' -> withForeignPtr fp $ \p -> do
            poke p' w
            S.c_intersperse (p' `plusPtr` 1) (p `plusPtr` o) (fromIntegral l) w

-- -- | The 'transpose' function transposes the rows and columns of its
-- -- 'ByteString' argument.
-- transpose :: [ByteString] -> [ByteString]
-- transpose css = L.map (\ss -> Chunk (S.pack ss) Empty)
--                       (L.transpose (L.map unpack css))
-- --TODO: make this fast
--
-- -- ---------------------------------------------------------------------
-- -- Reducing 'ByteString's
fold :: Monad m => (x -> Word8 -> x) -> x -> (x -> b) -> ByteString m () -> m b
fold step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! S.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty _      -> return (done x)
{-# INLINABLE fold #-}

--

-- | A strict version of 'foldl'.
-- foldl'           :: forall a b . (b -> a -> b) -> b -> [a] -> b
-- {-# INLINE foldl' #-}
-- foldl' k z0 xs =
--   foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> z `seq` fn (k z v))) (id :: b -> b) xs z0
--   -- See Note [Left folds via right fold]
fold'x :: Monad m => ByteString m r -> (x -> Word8 -> x) -> x -> (x -> b) -> m (b,r)
fold'x p0 = 
  dematerialize p0 
  (\r step begin done -> return (done begin, r)) 
  (\bs ff step begin done -> ff step (S.foldl' step begin bs) done)
  (\mf step begin done -> mf >>= \f -> f step begin done) 
  
fold' :: Monad m => (x -> Word8 -> x) -> x -> (x -> b) -> ByteString m r -> m (b,r)
fold' step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! S.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty r      -> return (done x,r)
{-# INLINABLE fold' #-}
-- -- | 'foldl', applied to a binary operator, a starting value (typically
-- -- the left-identity of the operator), and a ByteString, reduces the
-- -- ByteString using the binary operator, from left to right.
-- foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
-- foldl f z = go z
--   where go a Empty        = a
--         go a (Chunk c cs) = go (S.foldl f a c) cs
-- {-# INLINE foldl #-}
--
-- -- | 'foldl\'' is like 'foldl', but strict in the accumulator.
-- foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
-- foldl' f z = go z
--   where go !a Empty        = a
--         go !a (Chunk c cs) = go (S.foldl' f a c) cs
-- {-# INLINE foldl' #-}
--
-- -- | 'foldr', applied to a binary operator, a starting value
-- -- (typically the right-identity of the operator), and a ByteString,
-- -- reduces the ByteString using the binary operator, from right to left.
foldr :: Monad m => (Word8 -> a -> a) -> a -> ByteString m () -> m a
foldr k z cs = foldrChunks (flip (S.foldr k)) z cs
{-# INLINE foldr #-}
-- --
-- -- | 'foldl1' is a variant of 'foldl' that has no starting value
-- -- argument, and thus must be applied to non-empty 'ByteStrings'.
-- foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
-- foldl1 _ Empty        = errorEmptyList "foldl1"
-- foldl1 f (Chunk c cs) = foldl f (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)
--
-- -- | 'foldl1\'' is like 'foldl1', but strict in the accumulator.
-- foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
-- foldl1' _ Empty        = errorEmptyList "foldl1'"
-- foldl1' f (Chunk c cs) = foldl' f (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)
--
-- -- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- -- and thus must be applied to non-empty 'ByteString's
-- foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
-- foldr1 _ Empty          = errorEmptyList "foldr1"
-- foldr1 f (Chunk c0 cs0) = go c0 cs0
--   where go c Empty         = S.foldr1 f c
--         go c (Chunk c' cs) = S.foldr  f (go c' cs) c
--
-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: (Monad m) => [ByteString m ()] -> ByteString m ()
concat css0 = to css0
  where
    go css (Empty m')   = to css
    go css (Chunk c cs) = Chunk c (go css cs)
    go css (Go m)       = Go (liftM (go css) m)
    to []               = Empty ()
    to (cs:css)         = go css cs


-- -- | Map a function over a 'ByteString' and concatenate the results
-- concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
-- concatMap _ Empty        = Empty
-- concatMap f (Chunk c0 cs0) = to c0 cs0
--   where
--     go :: ByteString -> P.ByteString -> ByteString -> ByteString
--     go Empty        c' cs' = to c' cs'
--     go (Chunk c cs) c' cs' = Chunk c (go cs c' cs')
--
--     to :: P.ByteString -> ByteString -> ByteString
--     to c cs | S.null c  = case cs of
--         Empty          -> Empty
--         (Chunk c' cs') -> to c' cs'
--             | otherwise = go (f (S.unsafeHead c)) (S.unsafeTail c) cs
--
-- -- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- -- any element of the 'ByteString' satisfies the predicate.
-- any :: (Word8 -> Bool) -> ByteString -> Bool
-- any f cs = foldrChunks (\c rest -> S.any f c || rest) False cs
-- {-# INLINE any #-}
-- -- todo fuse
--
-- -- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- -- if all elements of the 'ByteString' satisfy the predicate.
-- all :: (Word8 -> Bool) -> ByteString -> Bool
-- all f cs = foldrChunks (\c rest -> S.all f c && rest) True cs
-- {-# INLINE all #-}
-- -- todo fuse
--
-- -- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
-- maximum :: ByteString -> Word8
-- maximum Empty        = errorEmptyList "maximum"
-- maximum (Chunk c cs) = foldlChunks (\n c' -> n `max` S.maximum c')
--                                    (S.maximum c) cs
-- {-# INLINE maximum #-}
--
-- -- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
-- minimum :: ByteString -> Word8
-- minimum Empty        = errorEmptyList "minimum"
-- minimum (Chunk c cs) = foldlChunks (\n c' -> n `min` S.minimum c')
--                                      (S.minimum c) cs
-- {-# INLINE minimum #-}
--
-- -- | The 'mapAccumL' function behaves like a combination of 'map' and
-- -- 'foldl'; it applies a function to each element of a ByteString,
-- -- passing an accumulating parameter from left to right, and returning a
-- -- final value of this accumulator together with the new ByteString.
-- mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
-- mapAccumL f s0 cs0 = go s0 cs0
--   where
--     go s Empty        = (s, Empty)
--     go s (Chunk c cs) = (s'', Chunk c' cs')
--         where (s',  c')  = S.mapAccumL f s c
--               (s'', cs') = go s' cs
--
-- -- | The 'mapAccumR' function behaves like a combination of 'map' and
-- -- 'foldr'; it applies a function to each element of a ByteString,
-- -- passing an accumulating parameter from right to left, and returning a
-- -- final value of this accumulator together with the new ByteString.
-- mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
-- mapAccumR f s0 cs0 = go s0 cs0
--   where
--     go s Empty        = (s, Empty)
--     go s (Chunk c cs) = (s'', Chunk c' cs')
--         where (s'', c') = S.mapAccumR f s' c
--               (s', cs') = go s cs
--
-- -- ---------------------------------------------------------------------
-- -- Building ByteStrings
--
-- -- | 'scanl' is similar to 'foldl', but returns a list of successive
-- -- reduced values from the left. This function will fuse.
-- --
-- -- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- --
-- -- Note that
-- --
-- -- > last (scanl f z xs) == foldl f z xs.
-- scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
-- scanl f z = snd . foldl k (z,singleton z)
--  where
--     k (c,acc) a = let n = f c a in (n, acc `snoc` n)
-- {-# INLINE scanl #-}
--
-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:

-- > iterate f x == [x, f x, f (f x), ...]

iterate :: (Word8 -> Word8) -> Word8 -> ByteString m ()
iterate f = unfoldr (\x -> case f x of !x' -> Just (x', x'))

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Word8 -> ByteString m ()
repeat w = cs where cs = Chunk (S.replicate BI.smallChunkSize w) cs

-- -- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- -- the value of every element.
-- --
-- replicate :: Int64 -> Word8 -> ByteString
-- replicate n w
--     | n <= 0             = Empty
--     | n < fromIntegral smallChunkSize = Chunk (S.replicate (fromIntegral n) w) Empty
--     | r == 0             = cs -- preserve invariant
--     | otherwise          = Chunk (S.unsafeTake (fromIntegral r) c) cs
--  where
--     c      = S.replicate smallChunkSize w
--     cs     = nChunks q
--     (q, r) = quotRem n (fromIntegral smallChunkSize)
--     nChunks 0 = Empty
--     nChunks m = Chunk c (nChunks (m-1))

-- | 'cycle' ties a finite ByteString into a circular one, or equivalently,
-- the infinite repetition of the original ByteString.
--
cycle :: Monad m => ByteString m r -> ByteString m s
cycle (Empty _) = error "cycle" -- errorEmptyList "cycle"
cycle cs    = cs >> cycle cs -- ' where cs' = foldrChunks Chunk cs' cs

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString m ()
unfoldr f s0 = unfoldChunk 32 s0
  where unfoldChunk n s =
          case S.unfoldrN n f s of
            (c, Nothing)
              | S.null c  -> Empty ()
              | otherwise -> Chunk c (Empty ())
            (c, Just s')  -> Chunk c (unfoldChunk (n*2) s')
--
-- unfoldr' :: (a -> Maybe (Word8, a)) -> a -> P.ByteString
-- unfoldr' f = Type.concat . unfoldChunk 32 64
--   where unfoldChunk n n' x =
--           case unfoldrN' n f x of
--             (s, Nothing) -> s : []
--             (s, Just x') -> s : unfoldChunk n' (n+n') x'
-- {-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
-- unfoldrN' :: Int -> (a -> Maybe (Word8, a)) -> a -> (P.ByteString, Maybe a)
-- unfoldrN' i f x0
--     | i < 0     = (empty, Just x0)
--     | otherwise = unsafePerformIO $ S.createAndTrim' i $ \p -> go p x0 0
--   where
--     go !p !x !n
--       | n == i    = return (0, n, Just x)
--       | otherwise = case f x of
--                       Nothing     -> return (0, n, Nothing)
--                       Just (w,x') -> do poke p w
--                                         go (p `plusPtr` 1) x' (n+1)
-- {-# INLINE unfoldrN' #-}
-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n\/c)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Monad m => Int64 -> ByteString m r -> ByteString m ()
take i _ | i <= 0 = Empty ()
take i cs0         = take' i cs0
  where take' 0 _            = Empty ()
        take' _ (Empty _)    = Empty ()
        take' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.take (fromIntegral n) c) (Empty ())
            else Chunk c (take' (n - fromIntegral (S.length c)) cs)
        take' n (Go m) = Go (liftM (take' n) m)

-- | /O(n\/c)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Monad m => Int64 -> ByteString m r -> ByteString m r
drop i p | i <= 0 = p
drop i cs0 = drop' i cs0
  where drop' 0 cs           = cs
        drop' _ (Empty r)    = Empty r
        drop' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.drop (fromIntegral n) c) cs
            else drop' (n - fromIntegral (S.length c)) cs
        drop' n (Go m) = Go (liftM (drop' n) m)
-- | /O(n\/c)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Monad m => Int64 -> ByteString m r -> ByteString m (ByteString m r)
splitAt i cs0 | i <= 0 = Empty cs0
splitAt i cs0 = splitAt' i cs0
  where splitAt' 0 cs           = Empty cs
        splitAt' _ (Empty r  )   = Empty (Empty r)
        splitAt' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.take (fromIntegral n) c) $ 
                     Empty (Chunk (S.drop (fromIntegral n) c) cs)
            else Chunk c (splitAt' (n - fromIntegral (S.length c)) cs)
        splitAt' n (Go m) = Go  (liftM (splitAt' n) m)

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> ByteString m r -> ByteString m ()
takeWhile f cs0 = takeWhile' cs0
  where takeWhile' (Empty _)        = Empty ()
        takeWhile' (Chunk c cs) =
          case findIndexOrEnd (not . f) c of
            0                  -> Empty ()
            n | n < S.length c -> Chunk (S.take n c) (Empty ())
              | otherwise      -> Chunk c (takeWhile' cs)

-- -- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
-- dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
-- dropWhile f cs0 = dropWhile' cs0
--   where dropWhile' Empty        = Empty
--         dropWhile' (Chunk c cs) =
--           case findIndexOrEnd (not . f) c of
--             n | n < S.length c -> Chunk (S.drop n c) cs
--               | otherwise      -> dropWhile' cs

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: Monad m => (Word8 -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
break f cs0 = break' cs0
  where break' (Empty r)        = Empty (Empty r)
        break' (Chunk c cs) =
          case findIndexOrEnd f c of
            0                  -> Empty (Chunk c cs)
            n | n < S.length c -> Chunk (S.take n c) $ 
                                      Empty (Chunk (S.drop n c) cs)
              | otherwise      -> Chunk c (break' cs)
        break' (Go m) = Go (liftM break' m)

--
-- -- TODO
-- --
-- -- Add rules
-- --
--
-- {-
-- -- | 'breakByte' breaks its ByteString argument at the first occurence
-- -- of the specified byte. It is more efficient than 'break' as it is
-- -- implemented with @memchr(3)@. I.e.
-- --
-- -- > break (=='c') "abcd" == breakByte 'c' "abcd"
-- --
-- breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
-- breakByte c (LPS ps) = case (breakByte' ps) of (a,b) -> (LPS a, LPS b)
--   where breakByte' []     = ([], [])
--         breakByte' (x:xs) =
--           case P.elemIndex c x of
--             Just 0  -> ([], x : xs)
--             Just n  -> (P.take n x : [], P.drop n x : xs)
--             Nothing -> let (xs', xs'') = breakByte' xs
--                         in (x : xs', xs'')
--
-- -- | 'spanByte' breaks its ByteString argument at the first
-- -- occurence of a byte other than its argument. It is more efficient
-- -- than 'span (==)'
-- --
-- -- > span  (=='c') "abcd" == spanByte 'c' "abcd"
-- --
-- spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
-- spanByte c (LPS ps) = case (spanByte' ps) of (a,b) -> (LPS a, LPS b)
--   where spanByte' []     = ([], [])
--         spanByte' (x:xs) =
--           case P.spanByte c x of
--             (x', x'') | P.null x'  -> ([], x : xs)
--                       | P.null x'' -> let (xs', xs'') = spanByte' xs
--                                        in (x : xs', xs'')
--                       | otherwise  -> (x' : [], x'' : xs)
-- -}
--
-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: Monad m => (Word8 -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
span p = break (not . p)

-- -- | /O(n)/ Splits a 'ByteString' into components delimited by
-- -- separators, where the predicate returns True for a separator element.
-- -- The resulting components do not contain the separators.  Two adjacent
-- -- separators result in an empty component in the output.  eg.
-- --
-- -- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- -- > splitWith (=='a') []        == []
-- --
splitWith :: Monad m => (Word8 -> Bool) -> ByteString m r -> List (ByteString m) m r
splitWith _ (Empty r)      = Return r
splitWith p (Chunk c0 cs0) = comb [] (S.splitWith p c0) cs0

   where -- comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
          comb acc (s:[]) (Empty r)    = Step (revChunks (s:acc) (Return r))
          comb acc (s:[]) (Chunk c cs) = comb (s:acc) (S.splitWith p c) cs
          comb acc b (Go m)            = Wrap (liftM (comb acc b) m)
          comb acc (s:ss) cs           = Step (revChunks (s:acc) (comb [] ss cs))

{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteStrings' that
-- are slices of the original.
--
split :: Monad m => Word8 -> ByteString m r -> List (ByteString m) m r
split w = loop SPEC 
  where
  loop !_ x = case x of 
    Empty r      ->  Return r
    Go m         -> Wrap $ liftM (loop SPEC) m
    Chunk c0 cs0 -> comb SPEC [] (S.split w c0) cs0
  comb !_ acc (s:[]) (Empty r)    = Step $ revChunks (s:acc) (Return r)
  comb !_ acc (s:[]) (Chunk c cs) = comb SPEC (s:acc) (S.split w c) cs
  comb !_ acc b (Go m)            = Wrap (liftM (comb SPEC acc b) m)
  comb !_ acc (s:ss) cs           = Step $ revChunks (s:acc) (comb SPEC [] ss cs)
{-# INLINE split #-}
--
-- | The 'group' function take`5s a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
group :: Monad m => ByteString m r -> List (ByteString m) m r
group = go
  where
  go (Empty r)       = Return r
  go (Chunk c cs)
    | S.length c == 1  = Step $ to [c] (S.unsafeHead c) cs
    | otherwise        = Step $ to [S.unsafeTake 1 c] (S.unsafeHead c)
                                     (Chunk (S.unsafeTail c) cs)

  to acc !_ (Empty r)        = revNonEmptyChunks acc  (Empty (Return r))
  to acc !w (Chunk c cs) =
    case findIndexOrEnd (/= w) c of
      0                    -> revNonEmptyChunks acc (Empty (go (Chunk c cs)))
      n | n == S.length c  -> to (S.unsafeTake n c : acc) w cs
        | otherwise        -> revNonEmptyChunks (S.unsafeTake n c : acc)
                                        (Empty (go (Chunk (S.unsafeDrop n c) cs)))

-- -- | The 'groupBy' function is the non-overloaded version of 'group'.
-- --
-- groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
-- groupBy k = go
--   where
--     go Empty        = []
--     go (Chunk c cs)
--       | S.length c == 1  = to [c] (S.unsafeHead c) cs
--       | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)
--
--     to acc !_ Empty        = revNonEmptyChunks acc : []
--     to acc !w (Chunk c cs) =
--       case findIndexOrEnd (not . k w) c of
--         0                    -> revNonEmptyChunks acc
--                               : go (Chunk c cs)
--         n | n == S.length c  -> to (S.unsafeTake n c : acc) w cs
--           | otherwise        -> revNonEmptyChunks (S.unsafeTake n c : acc)
--                               : go (Chunk (S.unsafeDrop n c) cs)
--
-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Monad m => ByteString m () -> List (ByteString m) m r -> ByteString m r
intercalate s (Return r) = Empty r
intercalate s (Wrap m) = Go $ liftM (intercalate s) m
intercalate s (Step bsls) = do  -- this isn't quite right yet
  ls <- bsls
  s >> loop ls
 where 
  loop (Return r) =  Empty r -- concat . (L.intersperse s)
  loop (Wrap m) = Go $ liftM loop m
  loop (Step bsls) = do   
    ls <- bsls
    case ls of 
      Return r -> Empty r  -- no '\n' before end, in this case.
      x -> s >> loop x

-- -- ---------------------------------------------------------------------
-- -- Indexing ByteStrings
--
-- -- | /O(c)/ 'ByteString' index (subscript) operator, starting from 0.
-- index :: ByteString -> Int64 -> Word8
-- index _  i | i < 0  = moduleError "index" ("negative index: " ++ show i)
-- index cs0 i         = index' cs0 i
--   where index' Empty     n = moduleError "index" ("index too large: " ++ show n)
--         index' (Chunk c cs) n
--           | n >= fromIntegral (S.length c) =
--               index' cs (n - fromIntegral (S.length c))
--           | otherwise       = S.unsafeIndex c (fromIntegral n)
--
-- -- | /O(n)/ The 'elemIndex' function returns the index of the first
-- -- element in the given 'ByteString' which is equal to the query
-- -- element, or 'Nothing' if there is no such element.
-- -- This implementation uses memchr(3).
-- elemIndex :: Word8 -> ByteString -> Maybe Int64
-- elemIndex w cs0 = elemIndex' 0 cs0
--   where elemIndex' _ Empty        = Nothing
--         elemIndex' n (Chunk c cs) =
--           case S.elemIndex w c of
--             Nothing -> elemIndex' (n + fromIntegral (S.length c)) cs
--             Just i  -> Just (n + fromIntegral i)
--
-- -- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- -- element in the given 'ByteString' which is equal to the query
-- -- element, or 'Nothing' if there is no such element. The following
-- -- holds:
-- --
-- -- > elemIndexEnd c xs ==
-- -- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
-- elemIndexEnd :: Word8 -> ByteString -> Maybe Int64
-- elemIndexEnd w = elemIndexEnd' 0
--   where
--     elemIndexEnd' _ Empty = Nothing
--     elemIndexEnd' n (Chunk c cs) =
--       let !n' = n + S.length c
--           !i  = fmap (fromIntegral . (n +)) $ S.elemIndexEnd w c
--       in elemIndexEnd' n' cs `mplus` i
--
-- -- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- -- the indices of all elements equal to the query element, in ascending order.
-- -- This implementation uses memchr(3).
-- elemIndices :: Word8 -> ByteString -> [Int64]
-- elemIndices w cs0 = elemIndices' 0 cs0
--   where elemIndices' _ Empty        = []
--         elemIndices' n (Chunk c cs) = L.map ((+n).fromIntegral) (S.elemIndices w c)
--                              ++ elemIndices' (n + fromIntegral (S.length c)) cs
--
-- -- | count returns the number of times its argument appears in the ByteString
-- --
-- -- > count = length . elemIndices
-- --
-- -- But more efficiently than using length on the intermediate list.
-- count :: Word8 -> ByteString -> Int64
-- count w cs = foldlChunks (\n c -> n + fromIntegral (S.count w c)) 0 cs
--
-- -- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- -- returns the index of the first element in the ByteString
-- -- satisfying the predicate.
-- findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
-- findIndex k cs0 = findIndex' 0 cs0
--   where findIndex' _ Empty        = Nothing
--         findIndex' n (Chunk c cs) =
--           case S.findIndex k c of
--             Nothing -> findIndex' (n + fromIntegral (S.length c)) cs
--             Just i  -> Just (n + fromIntegral i)
-- {-# INLINE findIndex #-}
--
-- -- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- -- and returns the first element in matching the predicate, or 'Nothing'
-- -- if there is no such element.
-- --
-- -- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
-- --
-- find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
-- find f cs0 = find' cs0
--   where find' Empty        = Nothing
--         find' (Chunk c cs) = case S.find f c of
--             Nothing -> find' cs
--             Just w  -> Just w
-- {-# INLINE find #-}
--
-- -- | The 'findIndices' function extends 'findIndex', by returning the
-- -- indices of all elements satisfying the predicate, in ascending order.
-- findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
-- findIndices k cs0 = findIndices' 0 cs0
--   where findIndices' _ Empty        = []
--         findIndices' n (Chunk c cs) = L.map ((+n).fromIntegral) (S.findIndices k c)
--                              ++ findIndices' (n + fromIntegral (S.length c)) cs
--
-- -- ---------------------------------------------------------------------
-- -- Searching ByteStrings
--
-- -- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
-- elem :: Word8 -> ByteString -> Bool
-- elem w cs = case elemIndex w cs of Nothing -> False ; _ -> True
--
-- -- | /O(n)/ 'notElem' is the inverse of 'elem'
-- notElem :: Word8 -> ByteString -> Bool
-- notElem w cs = not (elem w cs)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString m r -> ByteString m r
filter p s = go s
    where
        go (Empty r )   = Empty r
        go (Chunk x xs) = chunk (S.filter p x) (go xs)
{-# INLINE filter #-}
--
-- {-
-- -- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- -- (==)/, for the common case of filtering a single byte. It is more
-- -- efficient to use /filterByte/ in this case.
-- --
-- -- > filterByte == filter . (==)
-- --
-- -- filterByte is around 10x faster, and uses much less space, than its
-- -- filter equivalent
-- filterByte :: Word8 -> ByteString -> ByteString
-- filterByte w ps = replicate (count w ps) w
-- {-# INLINE filterByte #-}
--
-- {-# RULES
-- "ByteString specialise filter (== x)" forall x.
--   filter ((==) x) = filterByte x
--
-- "ByteString specialise filter (== x)" forall x.
--  filter (== x) = filterByte x
--   #-}
-- -}
--
-- {-
-- -- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- -- case of filtering a single byte out of a list. It is more efficient
-- -- to use /filterNotByte/ in this case.
-- --
-- -- > filterNotByte == filter . (/=)
-- --
-- -- filterNotByte is around 2x faster than its filter equivalent.
-- filterNotByte :: Word8 -> ByteString -> ByteString
-- filterNotByte w (LPS xs) = LPS (filterMap (P.filterNotByte w) xs)
-- -}
--
-- -- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- -- the pair of ByteStrings with elements which do and do not satisfy the
-- -- predicate, respectively; i.e.,
-- --
-- -- > partition p bs == (filter p xs, filter (not . p) xs)
-- --
-- partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
-- partition f p = (filter f p, filter (not . f) p)
-- --TODO: use a better implementation
--
-- -- ---------------------------------------------------------------------
-- -- Searching for substrings
--
-- -- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- -- iff the first is a prefix of the second.
-- isPrefixOf :: ByteString -> ByteString -> Bool
-- isPrefixOf Empty _  = True
-- isPrefixOf _ Empty  = False
-- isPrefixOf (Chunk x xs) (Chunk y ys)
--     | S.length x == S.length y = x == y  && isPrefixOf xs ys
--     | S.length x <  S.length y = x == yh && isPrefixOf xs (Chunk yt ys)
--     | otherwise                = xh == y && isPrefixOf (Chunk xt xs) ys
--   where (xh,xt) = S.splitAt (S.length y) x
--         (yh,yt) = S.splitAt (S.length x) y
--
-- -- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- -- iff the first is a suffix of the second.
-- --
-- -- The following holds:
-- --
-- -- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
-- --
-- isSuffixOf :: ByteString -> ByteString -> Bool
-- isSuffixOf x y = reverse x `isPrefixOf` reverse y
-- --TODO: a better implementation
--
-- -- ---------------------------------------------------------------------
-- -- Zipping
--
-- -- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- -- corresponding pairs of bytes. If one input ByteString is short,
-- -- excess elements of the longer ByteString are discarded. This is
-- -- equivalent to a pair of 'unpack' operations.
-- zip :: ByteString -> ByteString -> [(Word8,Word8)]
-- zip = zipWith (,)
--
-- -- | 'zipWith' generalises 'zip' by zipping with the function given as
-- -- the first argument, instead of a tupling function.  For example,
-- -- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- -- corresponding sums.
-- zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
-- zipWith _ Empty     _  = []
-- zipWith _ _      Empty = []
-- zipWith f (Chunk a as) (Chunk b bs) = go a as b bs
--   where
--     go x xs y ys = f (S.unsafeHead x) (S.unsafeHead y)
--                  : to (S.unsafeTail x) xs (S.unsafeTail y) ys
--
--     to x Empty         _ _             | S.null x       = []
--     to _ _             y Empty         | S.null y       = []
--     to x xs            y ys            | not (S.null x)
--                                       && not (S.null y) = go x  xs y  ys
--     to x xs            _ (Chunk y' ys) | not (S.null x) = go x  xs y' ys
--     to _ (Chunk x' xs) y ys            | not (S.null y) = go x' xs y  ys
--     to _ (Chunk x' xs) _ (Chunk y' ys)                  = go x' xs y' ys
--
-- -- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- -- ByteStrings. Note that this performs two 'pack' operations.
-- unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
-- unzip ls = (pack (L.map fst ls), pack (L.map snd ls))
-- {-# INLINE unzip #-}
--
-- -- ---------------------------------------------------------------------
-- -- Special lists
--
-- -- | /O(n)/ Return all initial segments of the given 'ByteString', shortest first.
-- inits :: ByteString -> [ByteString]
-- inits = (Empty :) . inits'
--   where inits' Empty        = []
--         inits' (Chunk c cs) = L.map (\c' -> Chunk c' Empty) (L.tail (S.inits c))
--                            ++ L.map (Chunk c) (inits' cs)
--
-- -- | /O(n)/ Return all final segments of the given 'ByteString', longest first.
-- tails :: ByteString -> [ByteString]
-- tails Empty         = Empty : []
-- tails cs@(Chunk c cs')
--   | S.length c == 1 = cs : tails cs'
--   | otherwise       = cs : tails (Chunk (S.unsafeTail c) cs')
--
-- -- ---------------------------------------------------------------------
-- -- Low level constructors
--
-- -- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
-- --   This is mainly useful to allow the rest of the data pointed
-- --   to by the 'ByteString' to be garbage collected, for example
-- --   if a large string has been read in, and only a small part of it
-- --   is needed in the rest of the program.
-- copy :: ByteString -> ByteString
-- copy cs = foldrChunks (Chunk . S.copy) Empty cs
-- --TODO, we could coalese small blocks here
-- --FIXME: probably not strict enough, if we're doing this to avoid retaining
-- -- the parent blocks then we'd better copy strictly.
--
-- -- ---------------------------------------------------------------------
--
-- -- TODO defrag func that concatenates block together that are below a threshold
-- -- defrag :: ByteString -> ByteString
--
-- -- ---------------------------------------------------------------------
-- -- Lazy ByteString IO
-- --
-- -- Rule for when to close: is it expected to read the whole file?
-- -- If so, close when done.
-- --
--
-- -- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- -- are read on demand, in at most @k@-sized chunks. It does not block
-- -- waiting for a whole @k@-sized chunk, so if less than @k@ bytes are
-- -- available then they will be returned immediately as a smaller chunk.
-- --
-- -- The handle is closed on EOF.
-- --
-- -- Note: the 'Handle' should be placed in binary mode with
-- -- 'System.IO.hSetBinaryMode' for 'hGetContentsN' to
-- -- work correctly.
--
hGetContentsN :: Int -> Handle ->  ByteString IO ()
hGetContentsN k h = loop -- TODO close on exceptions
  where
--    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- liftIO (S.hGetSome h k )-- only blocks if there is no data available
        if S.null c
          then Go $ do 
             hClose h
             return (Empty ())
          else Chunk c loop 

-- | Read @n@ bytes into a 'ByteString', directly from the
-- specified 'Handle', in chunks of size @k@.
--
hGetN :: Int -> Handle -> Int -> ByteString IO ()
hGetN k h n | n > 0 = readChunks n
  where
    readChunks !i = Go $ do
        c <- S.hGet h (min k i)
        case S.length c of
            0 ->  return $ Empty ()
            m -> return $ Chunk c (readChunks (i - m))

hGetN _ _ 0 = Empty ()
hGetN _ h n = liftIO $ illegalBufferSize h "hGet" n  -- <--- REPAIR !!!

-- | hGetNonBlockingN is similar to 'hGetContentsN', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available. Chunks are read on demand, in @k@-sized chunks.
--
hGetNonBlockingN :: Int -> Handle -> Int ->  ByteString IO ()
hGetNonBlockingN k h n | n > 0= readChunks n
  where
    readChunks !i = Go $ do
        c <- S.hGetNonBlocking h (min k i)
        case S.length c of
            0 -> return (Empty ())
            m -> return (Chunk c (readChunks (i - m)))

hGetNonBlockingN _ _ 0 = Empty ()
hGetNonBlockingN _ h n = liftIO $ illegalBufferSize h "hGetNonBlocking" n


illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, using the default chunk size.
--
-- Once EOF is encountered, the Handle is closed.
--
-- Note: the 'Handle' should be placed in binary mode with
-- 'System.IO.hSetBinaryMode' for 'hGetContents' to
-- work correctly.

hGetContents :: Handle -> ByteString IO ()
hGetContents = hGetContentsN defaultChunkSize
{-#INLINE hGetContents #-}

fromHandle  :: Handle -> ByteString IO ()
fromHandle = hGetContents
{-#INLINE fromHandle #-}

stdin :: ByteString IO ()
stdin =  hGetContents IO.stdin
{-#INLINE stdin #-}
-- | Read @n@ bytes into a 'ByteString', directly from the specified 'Handle'.
--
hGet :: Handle -> Int -> ByteString IO ()
hGet = hGetN defaultChunkSize
{-#INLINE hGet #-}
-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns 'empty'.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hGet'.
--
hGetNonBlocking :: Handle -> Int -> ByteString IO ()
hGetNonBlocking = hGetNonBlockingN defaultChunkSize
{-#INLINE hGetNonBlocking #-}
-- | Read an entire file /lazily/ into a 'ByteString'.
-- The Handle will be held open until EOF is encountered.
--
readFile :: FilePath -> ByteString IO ()
readFile f = Go $ liftM hGetContents (openBinaryFile f ReadMode) 
{-#INLINE readFile #-}
-- | Write a 'ByteString' to a file.
--
writeFile :: FilePath -> ByteString IO r -> IO r
writeFile f txt = bracket 
    (openBinaryFile f WriteMode) 
    hClose
    (\hdl -> hPut hdl txt)

-- | Append a 'ByteString' to a file.
--
appendFile :: FilePath -> ByteString IO r -> IO r
appendFile f txt = bracket 
    (openBinaryFile f AppendMode) 
    hClose
    (\hdl -> hPut hdl txt)

-- | getContents. Equivalent to hGetContents stdin. Will read /lazily/
--
getContents :: ByteString IO ()
getContents = hGetContents IO.stdin

-- | Outputs a 'ByteString' to the specified 'Handle'.
--
hPut :: Handle -> ByteString IO r -> IO r
hPut h cs = dematerialize cs return (\x y  -> S.hPut h x >> y) (>>= id)
{-#INLINE hPut #-}

-- | Pipes nomenclature for hPut
toHandle :: Handle -> ByteString IO r -> IO r
toHandle = hPut
{-#INLINE toHandle #-}

stdout :: ByteString IO r -> IO r
stdout = hPut IO.stdout
{-#INLINE stdout#-}

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString IO r -> ByteString IO r
hPutNonBlocking _ (Empty r)         = Empty r
hPutNonBlocking h bs@(Chunk c cs) = do
  c' <- lift $ S.hPutNonBlocking h c
  case S.length c' of
    l' | l' == S.length c -> hPutNonBlocking h cs
    0                     -> bs
    _                     -> Chunk c' cs

-- | A synonym for @hPut@, for compatibility
--
-- hPutStr :: Handle -> ByteString IO r -> IO r
-- hPutStr = hPut
--
-- -- | Write a ByteString to stdout
-- putStr :: ByteString IO r -> IO r
-- putStr = hPut IO.stdout

-- -- | Write a ByteString to stdout, appending a newline byte
-- --
-- putStrLn :: ByteString -> IO ()
-- putStrLn ps = hPut stdout ps >> hPut stdout (singleton 0x0a)
--
-- {-# DEPRECATED putStrLn
--     "Use Data.ByteString.Lazy.Char8.putStrLn instead. (Functions that rely on ASCII encodings belong in Data.ByteString.Lazy.Char8)"
--   #-}
--
-- -- | The interact function takes a function of type @ByteString -> ByteString@
-- -- as its argument. The entire input from the standard input device is passed
-- -- to th is function as its argument, and the resulting string is output on the
-- -- standard output device.
-- --
interact :: (ByteString IO () -> ByteString IO r) -> IO r
interact transformer = stdout (transformer stdin)

-- -- ---------------------------------------------------------------------
-- -- Internal utilities
--
-- -- Common up near identical calls to `error' to reduce the number
-- -- constant strings created when compiled:
-- errorEmptyList :: String -> a
-- errorEmptyList fun = moduleError fun "empty ByteString"
-- {-# NOINLINE errorEmptyList #-}
--
-- moduleError :: String -> String -> a
-- moduleError fun msg = error ("Data.ByteString.Lazy." ++ fun ++ ':':' ':msg)
-- {-# NOINLINE moduleError #-}

revNonEmptyChunks :: [P.ByteString] -> ByteString m r -> ByteString m r
revNonEmptyChunks xs p = loop p xs
  where 
    loop !bss [] = bss
    loop bss (b:bs) = loop (Chunk b bss) bs
-- L.foldl' (flip Chunk) Empty cs

-- reverse a list of possibly-empty chunks into a lazy ByteString
revChunks :: Monad m => [P.ByteString] -> r -> ByteString m r
revChunks cs r = L.foldl' (flip chunk) (Empty r) cs

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Word8 -> Bool) -> P.ByteString -> Int
findIndexOrEnd k (S.PS x s l) =
    S.accursedUnutterablePerformIO $
      withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go !ptr !n | n >= l    = return l
               | otherwise = do w <- peek ptr
                                if k w
                                  then return n
                                  else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}




zipWithList
  :: (Monad m)
  =>  (forall x . a -> ByteString m x -> ByteString m x)
  -> [a]
  -> List (ByteString m) m r
  -> List (ByteString m) m r
zipWithList op zs = loop zs
  where
    loop [] !ls      = loop zs ls
    loop a@(x:xs)  ls = case ls of 
      Return r -> Return r
      Step fls -> Step $ fmap (loop xs) (op x fls)
      Wrap mls -> Wrap $ liftM (loop a) mls

{-#INLINE zipWithList #-}
