{-# LANGUAGE CPP, BangPatterns #-}
{-#LANGUAGE RankNTypes, OverloadedStrings #-}
-- This library emulates Data.ByteString.Lazy.Char8 but includes a monadic element
-- and thus at certain points uses a `Stream`/`FreeT` type in place of lists.


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
    , unlinesIndividual
    , unwordsIndividual
    , singleton        -- singleton :: Monad m => Char -> ByteString m () 
    , fromChunks       -- fromChunks :: Monad m => Stream (Of ByteString) m r -> ByteString m r 
    , fromLazy         -- fromLazy :: Monad m => ByteString -> ByteString m () 
    , fromStrict       -- fromStrict :: ByteString -> ByteString m () 
    , toChunks         -- toChunks :: Monad m => ByteString m r -> Stream (Of ByteString) m r 
    , toLazy           -- toLazy :: Monad m => ByteString m () -> m ByteString 
    , toLazy'
    , toStrict         -- toStrict :: Monad m => ByteString m () -> m ByteString 
    , toStrict'
    , drain
    , wrap



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
    , head'            -- head' :: Monad m => ByteString m r -> m (Of Char r)
    , last             -- last :: Monad m => ByteString m r -> m Char
    , last'            -- last' :: Monad m => ByteString m r -> m (Of Char r)
    , null             -- null :: Monad m => ByteString m r -> m Bool 
    , null'            -- null' :: Monad m => ByteString m r -> m (Of Bool r)
    , uncons           -- uncons :: Monad m => ByteString m r -> m (Either r (Char, ByteString m r)) 
    , nextChar 
    
    -- * Direct chunk handling
    , unconsChunk
    , nextChunk        -- nextChunk :: Monad m => ByteString m r -> m (Either r (ByteString, ByteString m r)) 
    , consChunk
    , chunk
    , foldrChunks
    , foldlChunks
    
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
    , words
    , linesIndividual
    , wordsIndividual
    
    -- ** Special folds

    , concat          -- concat :: Monad m => Stream (ByteString m) m r -> ByteString m r 

    -- * Building ByteStrings

    -- ** Infinite ByteStrings
    , repeat           -- repeat :: Char -> ByteString m () 
    , iterate          -- iterate :: (Char -> Char) -> Char -> ByteString m () 
    , cycle            -- cycle :: Monad m => ByteString m r -> ByteString m s 

    -- ** Unfolding ByteStrings
    , unfoldr          -- unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString m () 
    , unfoldM          -- unfold  :: (a -> Either r (Char, a)) -> a -> ByteString m r

    -- *  Folds, including support for `Control.Foldl`
--    , foldr            -- foldr :: Monad m => (Char -> a -> a) -> a -> ByteString m () -> m a 
    , fold             -- fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b 
    , fold'            -- fold' :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (b, r) 
    , length
    , length'
    , count
    , count'
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

import Streaming hiding (concats, unfold, distribute, wrap)
import Streaming.Internal (Stream (..))
import qualified Streaming.Prelude as S
import qualified Streaming as S

import qualified Data.ByteString.Streaming as R
import Data.ByteString.Streaming.Internal

import Data.ByteString.Streaming
    (fromLazy, toLazy, toLazy', nextChunk, unconsChunk, 
    fromChunks, toChunks, fromStrict, toStrict, toStrict', 
    concat, distribute, drain,
    empty, null, null', length, length', append, cycle, 
    take, drop, splitAt, intercalate, group,
    appendFile, stdout, stdin, fromHandle, toHandle,
    hGetContents, hGetContentsN, hGet, hGetN, hPut, 
    getContents, hGetNonBlocking,
    hGetNonBlockingN, readFile, writeFile, interact)
 --   hPutNonBlocking, 

import Control.Monad            (liftM)
import System.IO                (Handle,openBinaryFile,IOMode(..)
                                ,hClose)
import qualified System.IO  as IO
import System.IO.Unsafe
import Control.Exception        (bracket)

import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import Data.Functor.Compose

unpack ::  Monad m => ByteString m r ->  Stream (Of Char) m r
unpack bs = case bs of 
    Empty r -> Return r
    Go m    -> Delay (liftM unpack m)
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
        . mapsM (liftM (\(str :> r) -> Char8.pack str :> r) . S.toListM') 
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
head :: Monad m => ByteString m r -> m Char
head = liftM (w2c) . R.head
{-# INLINE head #-}

-- | /O(1)/ Extract the first element of a ByteString, which may be non-empty
head' :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
head' = liftM (\(m:>r) -> fmap w2c m :> r) . R.head'
{-# INLINE head' #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite
-- and non-empty.
last :: Monad m => ByteString m r -> m Char
last = liftM (w2c) . R.last
{-# INLINE last #-}

last' :: Monad m => ByteString m r -> m (Of (Maybe Char) r)
last' = liftM (\(m:>r) -> fmap (w2c) m :> r) . R.last'
{-# INLINE last' #-}

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
fold :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m () -> m b
fold step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty _      -> return (done x)
{-# INLINABLE fold #-}


fold' :: Monad m => (x -> Char -> x) -> x -> (x -> b) -> ByteString m r -> m (Of b r)
fold' step begin done p0 = loop p0 begin
  where
    loop p !x = case p of
        Chunk bs bss -> loop bss $! Char8.foldl' step x bs
        Go    m    -> m >>= \p' -> loop p' x
        Empty r      -> return (done x :> r)
{-# INLINABLE fold' #-}
-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:

-- > iterate f x == [x, f x, f (f x), ...]

iterate :: (Char -> Char) -> Char -> ByteString m r
iterate f c = R.iterate (c2w . f . w2c) (c2w c)

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Char -> ByteString m r
repeat = R.repeat . c2w

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


unfoldr :: (a -> Either r (Char, a)) -> a -> ByteString m r
unfoldr step = R.unfoldr (either Left (\(c,a) -> Right (c2w c,a)) . step) 


-- ---------------------------------------------------------------------


-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m ()
takeWhile f  = R.takeWhile (f . w2c)
-- -- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
-- dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
-- dropWhile f cs0 = dropWhile' cs0
--   where dropWhile' Empty        = Empty
--         dropWhile' (Chunk c cs) =
--           case findIndexOrEnd (not . f) c of
--             n | n < B.length c -> Chunk (B.drop n c) cs
--               | otherwise      -> dropWhile' cs

{- | 'break' @p@ is equivalent to @'span' ('not' . p)@.

-}
break :: Monad m => (Char -> Bool) -> ByteString m r -> ByteString m (ByteString m r)
break f = R.break (f . w2c)
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
     thus never increases the use of memory. It is crucial to distinguish its
     type from that of 'linesIndividual'

> linesIndividual :: Monad m => ByteString m r -> Stream (Of B.ByteString) m r
> lines :: Monad m => ByteString m r -> Stream (ByteString m) m r
-}

lines :: Monad m => ByteString m r -> Stream (ByteString m) m r
lines = R.split 10
{-#INLINE lines #-}

-- | The 'unlines' function restores line breaks between layers 
unlines :: Monad m => Stream (ByteString m) m r ->  ByteString m r
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
{-#INLINABLE unlines #-}

{-| 'linesIndividual' breaks streaming by concatening the chunks between line breaks

> linesIndividual = mapsM toStrict' . lines
-}
linesIndividual :: Monad m => ByteString m r -> Stream (Of B.ByteString) m r
linesIndividual = mapsM R.toStrict' . lines

-- | 
unlinesIndividual :: Monad m => Stream (Of B.ByteString) m r -> ByteString m r 
unlinesIndividual bss =  R.concat $ for bss (\bs -> layer $ R.chunk bs >> singleton '\n')

-- | 'words' breaks a byte stream up into a succession of byte streams 
--   corresponding to words, breaking Chars representing white space. This is 
--   the genuinely streaming 'words' to be distinguished from
--   'wordsIndividual', which will attempt to concatenate even infinitely
--   long words like @cycle "y"@ in memory.
words :: Monad m => ByteString m r -> Stream (ByteString m) m r
words =  filtered . R.splitWith B.isSpaceWord8 
 where 
  filtered stream = case stream of 
    Return r -> Return r
    Delay m -> Delay (liftM filtered m)
    Step bs -> Delay $ bs_loop bs 
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

{- | 'wordsIndividual' breaks a bytestream into a sequence of individual
     @Data.ByteString.ByteString@s, delimited by Chars representing white space. 
     It involves concatenation, of course, and is thus potentially unsafe.
     Distinguish the types

> wordsIndividual :: Monad m => ByteString m r  -> Stream (Of B.ByteString) m r
> words :: Monad m => ByteString m r -> Stream (ByteString m) m r

     The latter, genuinely streaming, 'words' can only break up chunks
     hidden in the stream that is given; the former potentially concatenates

> wordsIndividual = mapsM toStrict' . words

-}
wordsIndividual :: Monad m => ByteString m r  -> Stream (Of B.ByteString) m r
wordsIndividual = mapsM R.toStrict' . words


{- | 'unwordsIndividual' returns to a genuine bytestream by interspersing
     white space between a sequence of individual Data.ByteString.ByteString 
     Distinguish the types

> unwordsIndividual :: Monad m => Stream (Of B.ByteString) m r -> ByteString m r 
> unwords :: Monad m => Stream (ByteString m) m r -> ByteString m r

-}
unwordsIndividual :: Monad m => Stream (Of B.ByteString) m r -> ByteString m r 
unwordsIndividual bss =  R.concat $ for bss (\bs -> layer $ R.chunk bs >> singleton ' ')



string :: String -> ByteString m ()
string = chunk . B.pack . Prelude.map B.c2w
{-# INLINE string #-}


count :: Monad m => Char -> ByteString m r -> m Int
count c = R.count (c2w c)
{-# INLINE count #-}

count' :: Monad m => Char -> ByteString m r -> m (Of Int r)
count' c = R.count' (c2w c)
{-# INLINE count' #-}

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