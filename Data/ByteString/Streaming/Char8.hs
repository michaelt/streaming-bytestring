{-# LANGUAGE CPP, BangPatterns #-}
{-#LANGUAGE RankNTypes, OverloadedStrings #-}
-- This library emulates Data.ByteString.Lazy.Char8 but includes a monadic element
-- and thus at certain points uses a `Stream`/`FreeT` type in place of lists.

-- |
-- Module      : Data.ByteString.Streaming.Char8
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
--               (c) Michael Thompson 2015
-- License     : BSD-style
--
-- Maintainer  : what_is_it_to_do_anything@yahoo.com
-- Stability   : experimental
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
-- Streaming variant by Michael Thompson, following the model of pipes-bytestring
--
module Data.ByteString.Streaming.Char8 (
    -- * The @ByteString@ type
    ByteString

    -- * Introducing and eliminating 'ByteString's 
    , empty            -- empty :: ByteString m () 
    , pack             -- pack :: Monad m => String -> ByteString m () 
    , string
    , unlines
    , singleton        -- singleton :: Monad m => Char -> ByteString m () 
    , fromChunks       -- fromChunks :: Monad m => Stream (Of ByteString) m r -> ByteString m r 
    , fromLazy         -- fromLazy :: Monad m => ByteString -> ByteString m () 
    , fromStrict       -- fromStrict :: ByteString -> ByteString m () 
    , toChunks         -- toChunks :: Monad m => ByteString m r -> Stream (Of ByteString) m r 
    , toLazy           -- toLazy :: Monad m => ByteString m () -> m ByteString 
    , toStrict         -- toStrict :: Monad m => ByteString m () -> m ByteString 

    -- * Transforming ByteStrings
    , map              -- map :: Monad m => (Char -> Char) -> ByteString m r -> ByteString m r 
    , intercalate      -- intercalate :: Monad m => ByteString m () -> Stream (ByteString m) m r -> ByteString m r 
    , intersperse      -- intersperse :: Monad m => Char -> ByteString m r -> ByteString m r 

    -- * Basic interface
    , cons             -- cons :: Monad m => Char -> ByteString m r -> ByteString m r 
    , cons'            -- cons' :: Char -> ByteString m r -> ByteString m r 
    , append           -- append :: Monad m => ByteString m r -> ByteString m s -> ByteString m s   
    , filter           -- filter :: (Char -> Bool) -> ByteString m r -> ByteString m r 
    , head             -- head :: Monad m => ByteString m r -> m Word8 
    , null             -- null :: Monad m => ByteString m r -> m Bool 
    , uncons           -- uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r)) 
    , nextChunk        -- nextChunk :: Monad m => ByteString m r -> m (Either r (ByteString, ByteString m r)) 

    -- * Substrings

    -- ** Breaking strings
    , break            -- break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r) 
    , drop             -- drop :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m r 
    , group            -- group :: Monad m => ByteString m r -> Stream (ByteString m) m r 
    , span             -- span :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r) 
    , splitAt          -- splitAt :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m (ByteString m r) 
    , splitWith        -- splitWith :: Monad m => (Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r 
    , take             -- take :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m () 
    , takeWhile        -- takeWhile :: (Char -> Bool) -> ByteString m r -> ByteString m () 

    -- ** Breaking into many substrings
    , split            -- split :: Monad m => Char -> ByteString m r -> Stream (ByteString m) m r 
    , lines
    
    -- ** Special folds

    , concat           -- concat :: Monad m => [ByteString m ()] -> ByteString m () 
    , concats          -- concats :: Monad m => Stream (ByteString m) m r -> ByteString m r 

    -- * Building ByteStrings

    -- ** Infinite ByteStrings
    , repeat           -- repeat :: Char -> ByteString m () 
    , iterate          -- iterate :: (Char -> Char) -> Char -> ByteString m () 
    , cycle            -- cycle :: Monad m => ByteString m r -> ByteString m s 

    -- ** Unfolding ByteStrings
    , unfoldr          -- unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString m () 
    , unfold           -- unfold  :: (a -> Either r (Char, a)) -> a -> ByteString m r

    -- *  Folds, including support for `Control.Foldl`
--    , foldr            -- foldr :: Monad m => (Char -> a -> a) -> a -> ByteString m () -> m a 
    , fold             -- fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b 
    , fold'            -- fold' :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (b, r) 

    -- * I\/O with 'ByteString's

    -- ** Standard input and output
    , getContents      -- getContents :: ByteString IO () 
    , stdin            -- stdin :: ByteString IO () 
    , stdout           -- stdout :: ByteString IO r -> IO r 
    , interact         -- interact :: (ByteString IO () -> ByteString IO r) -> IO r 

    -- ** Files
    , readFile         -- readFile :: FilePath -> ByteString IO () 
    , writeFile        -- writeFile :: FilePath -> ByteString IO r -> IO r 
    , appendFile       -- appendFile :: FilePath -> ByteString IO r -> IO r 

    -- ** I\/O with Handles
    , fromHandle       -- fromHandle :: Handle -> ByteString IO () 
    , toHandle         -- toHandle :: Handle -> ByteString IO r -> IO r 
    , hGet             -- hGet :: Handle -> Int -> ByteString IO () 
    , hGetContents     -- hGetContents :: Handle -> ByteString IO () 
    , hGetContentsN    -- hGetContentsN :: Int -> Handle -> ByteString IO () 
    , hGetN            -- hGetN :: Int -> Handle -> Int -> ByteString IO () 
    , hGetNonBlocking  -- hGetNonBlocking :: Handle -> Int -> ByteString IO () 
    , hGetNonBlockingN -- hGetNonBlockingN :: Int -> Handle -> Int -> ByteString IO () 
    , hPut             -- hPut :: Handle -> ByteString IO r -> IO r 
    , hPutNonBlocking  -- hPutNonBlocking :: Handle -> ByteString IO r -> ByteString IO r 
    -- * Etc.
    , zipWithStream    -- zipWithStream :: Monad m => (forall x. a -> ByteString m x -> ByteString m x) -> [a] -> Stream (ByteString m) m r -> Stream (ByteString m) m r 
    , distributed      -- distributed :: ByteString (t m) a -> t (ByteString m) a 
    , materialize
    , dematerialize
  ) where

import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)
import qualified Prelude
import qualified Data.List             as L  -- L for list/lazy
import qualified Data.ByteString.Lazy.Internal as BI  -- just for fromChunks etc


import qualified Data.ByteString        as P  (ByteString) -- type name only
import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal as S
import Data.ByteString.Internal (c2w,w2c)
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Char8 as Char8

import Data.ByteString.Streaming.Internal 
import Streaming hiding (concats, split, unfold)
import Streaming.Internal (Stream (..))

import qualified Data.ByteString.Streaming as BS
import Data.ByteString.Streaming (fromLazy, toLazy, nextChunk)
import Data.ByteString.Streaming.Internal

import Data.ByteString.Streaming
    (concats, distributed,
    fromHandle, fromChunks, toChunks, fromStrict, toStrict,
    empty, null, append, concat, cycle, 
    take, drop, splitAt, intercalate, group,
    appendFile, stdout, stdin, toHandle,
    hGetContents, hGetContentsN, hGet, hGetN, hPut, getContents, hGetNonBlocking,
    hGetNonBlockingN, readFile, writeFile,
    hPutNonBlocking, interact, zipWithStream)
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

data Char_ r = Char_ {-#UNPACK#-} !Char r

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

unpackAppendCharsLazy :: S.ByteString -> Stream Char_ m r -> Stream Char_ m r
unpackAppendCharsLazy (S.PS fp off len) xs
 | len <= 100 = unpackAppendCharsStrict (S.PS fp off len) xs
 | otherwise  = unpackAppendCharsStrict (S.PS fp off 100) remainder
 where
   remainder  = unpackAppendCharsLazy (S.PS fp (off+100) (len-100)) xs

unpackAppendCharsStrict :: S.ByteString -> Stream Char_ m r -> Stream Char_ m r
unpackAppendCharsStrict (S.PS fp off len) xs =
  S.accursedUnutterablePerformIO $ withForeignPtr fp $ \base -> do
       loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
   where
     loop !sentinal !p acc
       | p == sentinal = return acc
       | otherwise     = do x <- peek p
                            loop sentinal (p `plusPtr` (-1)) (Step (Char_ (S.w2c x) acc))



unpackChars ::  Monad m => ByteString m r ->  Stream Char_ m r
unpackChars (Empty r)    = Return r
unpackChars (Chunk c cs) = unpackAppendCharsLazy c (unpackChars cs)
unpackChars (Go m)       = Delay (liftM unpackChars m)


packChars :: Monad m => [Char] -> ByteString m ()
packChars cs0 =
    packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenChars n cs of
      (bs, [])  -> Chunk bs (Empty ())
      (bs, cs') -> Chunk bs  (packChunks (min (n * 2) BI.smallChunkSize) cs')


-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'.
pack :: Monad m => [Char] -> ByteString m ()
pack = packChars
{-#INLINE pack #-}

cons :: Monad m => Char -> ByteString m r -> ByteString m r
cons c cs = Chunk (S.singleton (c2w c)) cs
{-# INLINE cons #-}

singleton :: Monad m => Char -> ByteString m ()
singleton = BS.singleton . c2w
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
cons' :: Char -> ByteString m r -> ByteString m r
cons' c (Chunk bs bss) | S.length bs < 16 = Chunk (S.cons (c2w c) bs) bss
cons' c cs                             = Chunk (S.singleton (c2w c)) cs
{-# INLINE cons' #-}
-- --
-- -- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
-- snoc :: ByteString -> Word8 -> ByteString
-- snoc cs w = foldrChunks Chunk (singleton w) cs
-- {-# INLINE snoc #-}
--
-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: Monad m => ByteString m r -> m Char
head = liftM (w2c) . BS.head
{-# INLINE head #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
uncons (Empty r) = return (Left r)
uncons (Chunk c cs)
    = return $ Right (w2c (S.unsafeHead c)
                     , if S.length c == 1
                         then cs
                         else Chunk (S.unsafeTail c) cs )
uncons (Go m) = m >>= uncons
{-# INLINE uncons #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: Monad m => (Char -> Char) -> ByteString m r -> ByteString m r
map f = BS.map (c2w . f . w2c)
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
-- -- It is analogous to the intersperse function on Streams.
intersperse :: Monad m => Char -> ByteString m r -> ByteString m r
intersperse c = BS.intersperse (c2w c)
{-#INLINE intersperse #-}
-- -- | The 'transpose' function transposes the rows and columns of its
-- -- 'ByteString' argument.
-- transpose :: [ByteString] -> [ByteString]
-- transpose css = L.map (\ss -> Chunk (S.pack ss) Empty)
--                       (L.transpose (L.map unpack css))
-- --TODO: make this fast
--
-- -- ---------------------------------------------------------------------
-- -- Reducing 'ByteString's
fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b
fold step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty _      -> return (done x)
{-# INLINABLE fold #-}

--
fold' :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (b,r)
fold' step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty r      -> return (done x,r)
{-# INLINABLE fold' #-}
-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:

-- > iterate f x == [x, f x, f (f x), ...]

iterate :: (Char -> Char) -> Char -> ByteString m ()
iterate f c = BS.iterate (c2w . f . w2c) (c2w c)

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Char -> ByteString m ()
repeat = BS.repeat . c2w

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
-- | /O(n)/ The 'unfoldr' function is analogous to the Stream \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString m ()
unfoldr f = BS.unfoldr go where
  go a = case f a of
    Nothing -> Nothing
    Just (c,a) -> Just (c2w c, a)


unfold :: (a -> Either r (Char, a)) -> a -> ByteString m r
unfold step = BS.unfold (either Left (\(c,a) -> Right (c2w c,a)) . step) 


-- ---------------------------------------------------------------------


-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString m r -> ByteString m ()
takeWhile f  = BS.takeWhile (f . w2c)
-- -- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
-- dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
-- dropWhile f cs0 = dropWhile' cs0
--   where dropWhile' Empty        = Empty
--         dropWhile' (Chunk c cs) =
--           case findIndexOrEnd (not . f) c of
--             n | n < S.length c -> Chunk (S.drop n c) cs
--               | otherwise      -> dropWhile' cs

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
break f = BS.break (f . w2c)
--
-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
span p = break (not . p)

-- -- | /O(n)/ Splits a 'ByteString' into components delimited by
-- -- separators, where the predicate returns True for a separator element.
-- -- The resulting components do not contain the separators.  Two adjacent
-- -- separators result in an empty component in the output.  eg.
-- --
-- -- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- -- > splitWith (=='a') []        == []
-- --
splitWith :: Monad m => (Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r
splitWith f = BS.splitWith (f . w2c)
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
split :: Monad m => Char -> ByteString m r -> Stream (ByteString m) m r
split c = BS.split (c2w c)
{-# INLINE split #-}
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
filter :: (Char -> Bool) -> ByteString m r -> ByteString m r
filter p = BS.filter (p . w2c)
{-# INLINE filter #-}

unlines :: Monad m =>  Stream (ByteString m) m r ->  ByteString m r
unlines str =  case str of
  Return r -> Empty r
  Step bstr   -> do 
    st <- bstr 
    let bs = unlines st
    case bs of 
      Chunk "" (Empty r)   -> Empty r
      Chunk "\n" (Empty r) -> bs 
      _                    -> cons' '\n' bs
  Delay m  -> Go (liftM unlines m)
{-#INLINE unlines #-}

lines :: Monad m => ByteString m r -> Stream (ByteString m) m r
lines = BS.split 10
{-#INLINE lines #-}

string :: String -> ByteString m ()
string = yield . S.pack . Prelude.map S.c2w

