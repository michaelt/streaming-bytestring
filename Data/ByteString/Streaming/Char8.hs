{-# LANGUAGE CPP, BangPatterns #-}
{-#LANGUAGE RankNTypes, OverloadedStrings, ScopedTypeVariables #-}
-- | This library emulates "Data.ByteString.Lazy.Char8" but includes a monadic element
--   and thus at certain points uses a `Stream`/`FreeT` type in place of lists.
--   See the documentation for @Data.ByteString.Streaming@ and the examples of
--   of use to implement simple shell operations <https://gist.github.com/michaelt/6c6843e6dd8030e95d58 here>. Examples of use 
--   with @http-client@, @attoparsec@, @aeson@, @zlib@ etc. can be found in the
--   'streaming-utils' library.


module Data.ByteString.Streaming.Char8 (
    -- * The @ByteString@ type
    ByteString

    -- * Introducing and eliminating 'ByteString's 
    , empty            -- empty :: ByteString m () 
    , pack             -- pack :: Monad m => String -> ByteString m () 
    , unpack
    , string
    , unlines
    , unwords
    , singleton        -- singleton :: Monad m => Char -> ByteString m () 
    , fromChunks       -- fromChunks :: Monad m => Stream (Of ByteString) m r -> ByteString m r 
    , fromLazy         -- fromLazy :: Monad m => ByteString -> ByteString m () 
    , fromStrict       -- fromStrict :: ByteString -> ByteString m () 
    , toChunks         -- toChunks :: Monad m => ByteString m r -> Stream (Of ByteString) m r 
    , toLazy           -- toLazy :: Monad m => ByteString m () -> m ByteString 
    , toLazy_
    , toStrict         -- toStrict :: Monad m => ByteString m () -> m ByteString 
    , toStrict_
    , effects
    , copy
    , drained
    , mwrap


    -- * Transforming ByteStrings
    , map              -- map :: Monad m => (Char -> Char) -> ByteString m r -> ByteString m r 
    , intercalate      -- intercalate :: Monad m => ByteString m () -> Stream (ByteString m) m r -> ByteString m r 
    , intersperse      -- intersperse :: Monad m => Char -> ByteString m r -> ByteString m r 

    -- * Basic interface
    , cons             -- cons :: Monad m => Char -> ByteString m r -> ByteString m r 
    , cons'            -- cons' :: Char -> ByteString m r -> ByteString m r 
    , snoc
    , append           -- append :: Monad m => ByteString m r -> ByteString m s -> ByteString m s   
    , filter           -- filter :: (Char -> Bool) -> ByteString m r -> ByteString m r 
    , head             -- head :: Monad m => ByteString m r -> m Char
    , head_            -- head' :: Monad m => ByteString m r -> m (Of Char r)
    , last             -- last :: Monad m => ByteString m r -> m Char
    , last_            -- last' :: Monad m => ByteString m r -> m (Of Char r)
    , null             -- null :: Monad m => ByteString m r -> m Bool 
    , null_
    , testNull
    , nulls            -- null' :: Monad m => ByteString m r -> m (Of Bool r)
    , uncons           -- uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r)) 
    , nextChar 
    
    -- * Substrings

    -- ** Breaking strings
    , break            -- break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r) 
    , drop             -- drop :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m r 
    , dropWhile
    , group            -- group :: Monad m => ByteString m r -> Stream (ByteString m) m r 
    , groupBy
    , span             -- span :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r) 
    , splitAt          -- splitAt :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m (ByteString m r) 
    , splitWith        -- splitWith :: Monad m => (Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r 
    , take             -- take :: Monad m => GHC.Int.Int64 -> ByteString m r -> ByteString m () 
    , takeWhile        -- takeWhile :: (Char -> Bool) -> ByteString m r -> ByteString m () 

    -- ** Breaking into many substrings
    , split            -- split :: Monad m => Char -> ByteString m r -> Stream (ByteString m) m r 
    , lines
    , words
    , denull
    
    -- ** Special folds
    , concat          -- concat :: Monad m => Stream (ByteString m) m r -> ByteString m r 

    -- * Builders
    
    , toStreamingByteString
    , toStreamingByteStringWith
    , toBuilder
    , concatBuilders
    
    -- * Building ByteStrings

    -- ** Infinite ByteStrings
    , repeat           -- repeat :: Char -> ByteString m () 
    , iterate          -- iterate :: (Char -> Char) -> Char -> ByteString m () 
    , cycle            -- cycle :: Monad m => ByteString m r -> ByteString m s 

    -- ** Unfolding ByteStrings
    , unfoldr          -- unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString m () 
    , unfoldM          -- unfold  :: (a -> Either r (Char, a)) -> a -> ByteString m r
    , reread
    
    -- *  Folds, including support for `Control.Foldl`
--    , foldr            -- foldr :: Monad m => (Char -> a -> a) -> a -> ByteString m () -> m a 
    , fold             -- fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b 
    , fold_            -- fold' :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (b, r) 
    , length
    , length_
    , count
    , count_
    , readInt
    -- * I\/O with 'ByteString's

    -- ** Standard input and output
    , getContents      -- getContents :: ByteString IO () 
    , stdin            -- stdin :: ByteString IO () 
    , stdout           -- stdout :: ByteString IO r -> IO r 
    , interact         -- interact :: (ByteString IO () -> ByteString IO r) -> IO r 
    , putStr
    , putStrLn
    
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
--    , hPutNonBlocking  -- hPutNonBlocking :: Handle -> ByteString IO r -> ByteString IO r 

    -- * Simple chunkwise operations 
    , unconsChunk
    , nextChunk    
    , chunk
    , foldrChunks
    , foldlChunks
    , chunkFold
    , chunkFoldM
    , chunkMap
    , chunkMapM
    , chunkMapM_
    
    -- * Etc.
--    , zipWithStream    -- zipWithStream :: Monad m => (forall x. a -> ByteString m x -> ByteString m x) -> [a] -> Stream (ByteString m) m r -> Stream (ByteString m) m r 
    , distribute      -- distribute :: ByteString (t m) a -> t (ByteString m) a 
    , materialize
    , dematerialize
  ) where

import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,words, lines,foldl,foldr, unwords, unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)
import qualified Prelude

import qualified Data.ByteString        as B 
import qualified Data.ByteString.Internal as B
import Data.ByteString.Internal (c2w,w2c)
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as Char8

import Streaming hiding (concats, unfold, distribute, mwrap)
import Streaming.Internal (Stream (..))
import qualified Streaming.Prelude as S
import qualified Streaming as S

import qualified Data.ByteString.Streaming as R
import Data.ByteString.Streaming.Internal

import Data.ByteString.Streaming
    (fromLazy, toLazy, toLazy_, nextChunk, unconsChunk, 
    fromChunks, toChunks, fromStrict, toStrict, toStrict_, 
    concat, distribute, effects, drained, mwrap, toStreamingByteStringWith,
    toStreamingByteString, toBuilder, concatBuilders,
    empty, null, nulls, null_, testNull, length, length_, append, cycle, 
    take, drop, splitAt, intercalate, group, denull,
    appendFile, stdout, stdin, fromHandle, toHandle,
    hGetContents, hGetContentsN, hGet, hGetN, hPut, 
    getContents, hGetNonBlocking,
    hGetNonBlockingN, readFile, writeFile, interact,
    chunkFold, chunkFoldM, chunkMap, chunkMapM)
 --   hPutNonBlocking, 

import Control.Monad            (liftM)
import System.IO                (Handle,openBinaryFile,IOMode(..)
                                ,hClose)
import qualified System.IO  as IO
import System.IO.Unsafe
import Control.Exception        (bracket)
import Data.Char (isDigit)
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import Data.Functor.Compose
import Data.Functor.Sum
import qualified Data.List as L

unpack ::  Monad m => ByteString m r ->  Stream (Of Char) m r
unpack bs = case bs of 
    Empty r -> Return r
    Go m    -> Effect (liftM unpack m)
    Chunk c cs -> unpackAppendCharsLazy c (unpack cs)
  where 
  unpackAppendCharsLazy :: B.ByteString -> Stream (Of Char) m r -> Stream (Of Char) m r
  unpackAppendCharsLazy (B.PS fp off len) xs
   | len <= 100 = unpackAppendCharsStrict (B.PS fp off len) xs
   | otherwise  = unpackAppendCharsStrict (B.PS fp off 100) remainder
   where
     remainder  = unpackAppendCharsLazy (B.PS fp (off+100) (len-100)) xs

  unpackAppendCharsStrict :: B.ByteString -> Stream (Of Char) m r -> Stream (Of Char) m r
  unpackAppendCharsStrict (B.PS fp off len) xs =
    inlinePerformIO $ withForeignPtr fp $ \base -> do
         loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
     where
       loop !sentinal !p acc
         | p == sentinal = return acc
         | otherwise     = do x <- peek p
                              loop sentinal (p `plusPtr` (-1)) (Step (B.w2c x :> acc))
{-# INLINABLE unpack#-}
  

-- | /O(n)/ Convert a stream of separate characters into a packed byte stream.
pack :: Monad m => Stream (Of Char) m r -> ByteString m r
pack  = fromChunks 
        . mapped (liftM (\(str :> r) -> Char8.pack str :> r) . S.toList) 
        . chunksOf 32 
{-# INLINABLE pack #-}

-- | /O(1)/ Cons a 'Char' onto a byte stream.
cons :: Monad m => Char -> ByteString m r -> ByteString m r
cons c = R.cons (c2w c)
{-# INLINE cons #-}

-- | /O(1)/ Yield a 'Char' as a minimal 'ByteString'
singleton :: Monad m => Char -> ByteString m ()
singleton = R.singleton . c2w
{-# INLINE singleton #-}

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
cons' c (Chunk bs bss) | B.length bs < 16 = Chunk (B.cons (c2w c) bs) bss
cons' c cs                                = Chunk (B.singleton (c2w c)) cs
{-# INLINE cons' #-}
--
-- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
snoc :: Monad m => ByteString m r -> Char -> ByteString m r
snoc cs = R.snoc cs . c2w 
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head_ :: Monad m => ByteString m r -> m Char
head_ = liftM (w2c) . R.head_
{-# INLINE head_ #-}

-- | /O(1)/ Extract the first element of a ByteString, which may be non-empty
head :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
head = liftM (\(m:>r) -> fmap w2c m :> r) . R.head
{-# INLINE head #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite
-- and non-empty.
last_ :: Monad m => ByteString m r -> m Char
last_ = liftM (w2c) . R.last_
{-# INLINE last_ #-}

last :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
last = liftM (\(m:>r) -> fmap (w2c) m :> r) . R.last
{-# INLINE last #-}

groupBy :: Monad m => (Char -> Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r
groupBy rel = R.groupBy (\w w' -> rel (w2c w) (w2c w'))
{-#INLINE groupBy #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
uncons (Empty r) = return (Left r)
uncons (Chunk c cs)
    = return $ Right (w2c (B.unsafeHead c)
                     , if B.length c == 1
                         then cs
                         else Chunk (B.unsafeTail c) cs )
uncons (Go m) = m >>= uncons
{-# INLINABLE uncons #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: Monad m => (Char -> Char) -> ByteString m r -> ByteString m r
map f = R.map (c2w . f . w2c)
{-# INLINE map #-}
--
-- -- | /O(n)/ 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- reverse :: ByteString -> ByteString
-- reverse cs0 = rev Empty cs0
--   where rev a Empty        = a
--         rev a (Chunk c cs) = rev (Chunk (B.reverse c) a) cs
-- {-# INLINE reverse #-}
--
-- -- | The 'intersperse' function takes a 'Word8' and a 'ByteString' and
-- -- \`intersperses\' that byte between the elements of the 'ByteString'.
-- -- It is analogous to the intersperse function on Streams.
intersperse :: Monad m => Char -> ByteString m r -> ByteString m r
intersperse c = R.intersperse (c2w c)
{-#INLINE intersperse #-}
-- -- | The 'transpose' function transposes the rows and columns of its
-- -- 'ByteString' argument.
-- transpose :: [ByteString] -> [ByteString]
-- transpose css = L.map (\ss -> Chunk (B.pack ss) Empty)
--                       (L.transpose (L.map unpack css))
-- --TODO: make this fast
--
-- -- ---------------------------------------------------------------------
-- -- Reducing 'ByteString's
fold_ :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b
fold_ step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty _      -> return (done x)
{-# INLINABLE fold_ #-}


fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (Of b r)
fold step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty r      -> return (done x :> r)
{-# INLINABLE fold #-}
-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:

-- > iterate f x == [x, f x, f (f x), ...]

iterate :: (Char -> Char) -> Char -> ByteString m r
iterate f c = R.iterate (c2w . f . w2c) (c2w c)
{-#INLINE iterate #-}

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Char -> ByteString m r
repeat = R.repeat . c2w
{-#INLINE repeat #-}

-- -- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- -- the value of every element.
-- --
-- replicate :: Int64 -> Word8 -> ByteString
-- replicate n w
--     | n <= 0             = Empty
--     | n < fromIntegral smallChunkSize = Chunk (B.replicate (fromIntegral n) w) Empty
--     | r == 0             = cs -- preserve invariant
--     | otherwise          = Chunk (B.unsafeTake (fromIntegral r) c) cs
--  where
--     c      = B.replicate smallChunkSize w
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
unfoldM :: Monad m => (a -> Maybe (Char, a)) -> a -> ByteString m ()
unfoldM f = R.unfoldM go where
  go a = case f a of
    Nothing    -> Nothing
    Just (c,a) -> Just (c2w c, a)
{-#INLINE unfoldM #-}
    

unfoldr :: (a -> Either r (Char, a)) -> a -> ByteString m r
unfoldr step = R.unfoldr (either Left (\(c,a) -> Right (c2w c,a)) . step) 
{-#INLINE unfoldr #-}


-- ---------------------------------------------------------------------


-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m ()
takeWhile f  = R.takeWhile (f . w2c)
{-#INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.

dropWhile :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m r
dropWhile f = R.dropWhile (f . w2c)
{-#INLINE dropWhile #-}

{- | 'break' @p@ is equivalent to @'span' ('not' . p)@.

-}
break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
break f = R.break (f . w2c)
{-#INLINE break #-}

--
-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
span p = break (not . p)
{-#INLINE span #-}

-- -- | /O(n)/ Splits a 'ByteString' into components delimited by
-- -- separators, where the predicate returns True for a separator element.
-- -- The resulting components do not contain the separators.  Two adjacent
-- -- separators result in an empty component in the output.  eg.
-- --
-- -- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- -- > splitWith (=='a') []        == []
-- --
splitWith :: Monad m => (Char -> Bool) -> ByteString m r -> Stream (ByteString m) m r
splitWith f = R.splitWith (f . w2c)
{-# INLINE splitWith #-}

{- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
     argument, consuming the delimiter. I.e.

> split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
> split 'a'  "aXaXaXa"    == ["","X","X","X",""]
> split 'x'  "x"          == ["",""]

     and

> intercalate [c] . split c == id
> split == splitWith . (==)

As for all splitting functions in this library, this function does
not copy the substrings, it just constructs new 'ByteStrings' that
are slices of the original.

>>> Q.stdout $ Q.unlines $ Q.split 'n' "banana peel"
ba
a
a peel
-}
split :: Monad m => Char -> ByteString m r -> Stream (ByteString m) m r
split c = R.split (c2w c)
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
filter :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m r
filter p = R.filter (p . w2c)
{-# INLINE filter #-}



{- | 'lines' turns a ByteString into a connected stream of ByteStrings at
     divide at newline characters. The resulting strings do not contain newlines.
     This is the genuinely streaming 'lines' which only breaks chunks, and
     thus never increases the use of memory. 

     Because 'ByteString's are usually read in binary mode, with no line
     ending conversion, this function recognizes both @\\n@ and @\\r\\n@
     endings (regardless of the current platform).
-}

lines :: forall m r . Monad m => ByteString m r -> Stream (ByteString m) m r
lines text0 = loop1 text0
  where
    loop1 :: ByteString m r -> Stream (ByteString m) m r
    loop1 text =
      case text of
        Empty r -> Return r
        Go m -> Effect $ liftM loop1 m
        Chunk c cs
          | B.null c -> loop1 cs
          | otherwise -> Step (loop2 text)
    loop2 :: ByteString m r -> ByteString m (Stream (ByteString m) m r)
    loop2 text =
      case text of
        Empty r -> Empty (Return r)
        Go m -> Go $ liftM loop2 m
        Chunk c cs ->
          case B.elemIndex 10 c of
            Nothing -> Chunk c (loop2 cs)
            Just i ->
              let prefixLength =
                    if i >= 1 && B.unsafeIndex c (i-1) == 13 -- \r\n (dos)
                      then i-1
                      else i
                  rest =
                    if B.length c > i+1
                      then Chunk (B.drop (i+1) c) cs
                      else cs
              in Chunk (B.unsafeTake prefixLength c) (Empty (loop1 rest))
{-#INLINABLE lines #-}

-- | The 'unlines' function restores line breaks between layers.
--
-- Note that this is not a perfect inverse of 'lines':
--
--  * @'lines' . 'unlines'@ can produce more strings than there were if some of
--  the \"lines\" had embedded newlines.
--
--  * @'unlines' . 'lines'@ will replace @\\r\\n@ with @\\n@.
unlines :: Monad m => Stream (ByteString m) m r ->  ByteString m r
unlines = loop where
  loop str =  case str of
    Return r -> Empty r
    Step bstr   -> do 
      st <- bstr 
      let bs = unlines st
      case bs of 
        Chunk "" (Empty r)   -> Empty r
        Chunk "\n" (Empty r) -> bs 
        _                    -> cons' '\n' bs
    Effect m  -> Go (liftM unlines m)
{-#INLINABLE unlines #-}

-- | 'words' breaks a byte stream up into a succession of byte streams 
--   corresponding to words, breaking Chars representing white space. This is 
--   the genuinely streaming 'words'. A function that returns individual
--   strict bytestrings would concatenate even infinitely
--   long words like @cycle "y"@ in memory. It is best for the user who
--   has reflected on her materials to write `mapped toStrict . words` or the like,
--   if strict bytestrings are needed.
words :: Monad m => ByteString m r -> Stream (ByteString m) m r
words =  filtered . R.splitWith B.isSpaceWord8 
 where 
  filtered stream = case stream of 
    Return r -> Return r
    Effect m -> Effect (liftM filtered m)
    Step bs -> Effect $ bs_loop bs 
  bs_loop bs = case bs of
      Empty r -> return $ filtered r
      Go m ->  m >>= bs_loop
      Chunk b bs' -> if B.null b 
        then bs_loop bs'
        else return $ Step $ Chunk b (fmap filtered bs')
{-# INLINABLE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: Monad m => Stream (ByteString m) m r -> ByteString m r
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}




string :: String -> ByteString m ()
string = chunk . B.pack . Prelude.map B.c2w
{-# INLINE string #-}


count_ :: Monad m => Char -> ByteString m r -> m Int
count_ c = R.count_ (c2w c)
{-# INLINE count_ #-}

count :: Monad m => Char -> ByteString m r -> m (Of Int r)
count c = R.count (c2w c)
{-# INLINE count #-}

nextChar :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
nextChar b = do 
  e <- R.nextByte b
  case e of 
    Left r -> return $! Left r
    Right (w,bs) -> return $! Right (w2c w, bs)

putStr :: MonadIO m => ByteString m r -> m r
putStr = hPut IO.stdout
{-#INLINE putStr #-}

putStrLn :: MonadIO m => ByteString m r -> m r
putStrLn bs = hPut IO.stdout (snoc bs '\n')
{-#INLINE putStrLn #-}
-- , head'
-- , last
-- , last'
-- , length
-- , length'
-- , null
-- , null'
-- , count
-- , count'

{-| This will read positive or negative Ints that require 18 or fewer characters.
-}
readInt :: Monad m => ByteString m r -> m (Compose (Of (Maybe Int)) (ByteString m) r)
readInt = go . toStrict . splitAt 18 where
  go m = do 
    (bs :> rest) <- m
    case Char8.readInt bs of
      Nothing -> return (Compose (Nothing :> (chunk bs >> rest)))
      Just (n,more) -> if B.null more 
        then do 
          e <- uncons rest
          return $ case e of
            Left r -> Compose (Just n :> return r)
            Right (c,rest') -> if isDigit c 
               then Compose (Nothing :> (chunk bs >> cons' c rest'))
               else Compose (Just n :> (chunk more >> cons' c rest'))
        else return (Compose (Just n :> (chunk more >> rest)))
{-#INLINABLE readInt #-}

         -- uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r))
