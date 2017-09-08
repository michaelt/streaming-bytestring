{-# LANGUAGE CPP, BangPatterns, RankNTypes, GADTs #-}
{-# LANGUAGE UnliftedFFITypes, MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-} 

module Data.ByteString.Streaming.Internal (
   ByteString (..) 
   , consChunk             -- :: S.ByteString -> ByteString m r -> ByteString m r
   , chunkOverhead     -- :: Int
   , defaultChunkSize  -- :: Int
   , materialize       -- :: (forall x. (r -> x) -> (ByteString -> x -> x) -> (m x -> x) -> x) -> ByteString m r
   , dematerialize     -- :: Monad m =>  ByteString m r -> forall x.  (r -> x) -> (ByteString -> x -> x) -> (m x -> x) -> x
   , foldrChunks       -- :: Monad m =>  (ByteString -> a -> a) -> a -> ByteString m r -> m a
   , foldlChunks       -- :: Monad m =>  (a -> ByteString -> a) -> a -> ByteString m r -> m a

   , foldrChunksM       -- :: Monad m => (ByteString -> m a -> m a) -> m a -> ByteString m r -> m a
   , foldlChunksM       -- :: Monad m => (ByteString -> m a -> m a) -> m a -> ByteString m r -> m a
   , chunkFold
   , chunkFoldM
   , chunkMap
   , chunkMapM
   , chunkMapM_
   , unfoldMChunks
   , unfoldrChunks
   
   , packChars
   , smallChunkSize     -- :: Int
   , unpackBytes        -- :: Monad m => ByteString m r -> Stream Word8_ m r
   , packBytes
   , chunk             --  :: ByteString -> ByteString m ()
   , mwrap 
   , unfoldrNE
   , reread
   , inlinePerformIO
   , unsafeLast
   , unsafeInit
   , copy
   
   -- * ResourceT help
   , bracketByteString
  ) where

import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)
import qualified Prelude
import Control.Monad.Trans
import Control.Monad
import Control.Applicative
import Control.Monad.Morph
import Data.Monoid (Monoid(..))

import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal as S

import Streaming (Of(..))
import Streaming.Internal hiding (concats, mwrap, step)
import qualified Streaming.Prelude as SP

import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts ( SpecConstrAnnotation(..) )
import Data.String

import Data.Functor.Identity
import Data.Word
import System.IO.Unsafe
import GHC.Base                 (realWorld#,unsafeChr)
import GHC.IO                   (IO(IO))

import Control.Monad.Base
import Control.Monad.Trans.Resource
import Control.Monad.Catch (MonadCatch (..))

-- | A space-efficient representation of a succession of 'Word8' vectors, supporting many
-- efficient operations.
--
-- An effectful 'ByteString' contains 8-bit bytes, or by using the operations
-- from "Data.ByteString.Streaming.Char8" it can be interpreted as containing
-- 8-bit characters.

data ByteString m r =
  Empty r
  | Chunk {-#UNPACK #-} !S.ByteString (ByteString m r )
  | Go (m (ByteString m r ))

instance Monad m => Functor (ByteString m) where
  fmap f x = case x of
    Empty a      -> Empty (f a)
    Chunk bs bss -> Chunk bs (fmap f bss)
    Go mbss      -> Go (liftM (fmap f) mbss)

instance Monad m => Applicative (ByteString m) where
  pure = Empty
  {-#INLINE pure #-} 
  bf <*> bx = do {f <- bf; x <- bx; Empty (f x)}
  {-#INLINE (<*>) #-} 
  (*>) = (>>)
  {-#INLINE (*>) #-}
  
instance Monad m => Monad (ByteString m) where
  return = Empty
  {-#INLINE return #-}
  x0 >> y = loop SPEC x0 where
    loop !_ x = case x of   -- this seems to be insanely effective
      Empty _ -> y
      Chunk a b -> Chunk a (loop SPEC b)
      Go m -> Go (liftM (loop SPEC) m)
  {-#INLINEABLE (>>)#-}
  x >>= f =
    -- case x of
    --   Empty a -> f a
    --   Chunk bs bss -> Chunk bs (bss >>= f)
    --   Go mbss      -> Go (liftM (>>= f) mbss)
    loop SPEC2 x where -- unlike >> this SPEC seems pointless 
      loop !_ y = case y of
        Empty a -> f a
        Chunk bs bss -> Chunk bs (loop SPEC bss)
        Go mbss      -> Go (liftM (loop SPEC) mbss)
  {-#INLINEABLE (>>=) #-}
  
instance MonadIO m => MonadIO (ByteString m) where
  liftIO io = Go (liftM Empty (liftIO io))
  {-#INLINE liftIO #-}

instance MonadTrans ByteString where
  lift ma = Go $ liftM Empty ma
  {-#INLINE lift #-}

instance MFunctor ByteString where
  hoist phi bs = case bs of
    Empty r        -> Empty r
    Chunk bs' rest -> Chunk bs' (hoist phi rest)
    Go m           -> Go (phi (liftM (hoist phi) m))
  {-#INLINABLE hoist #-}
  
instance (r ~ ()) => IsString (ByteString m r) where
  fromString = chunk . S.pack . Prelude.map S.c2w
  {-#INLINE fromString #-}
  
instance (m ~ Identity, Show r) => Show (ByteString m r) where
  show bs0 = case bs0 of  -- the implementation this instance deserves ...
    Empty r           -> "Empty (" ++ show r ++ ")"
    Go (Identity bs') -> "Go (Identity (" ++ show bs' ++ "))"
    Chunk bs'' bs     -> "Chunk " ++ show bs'' ++ " (" ++ show bs ++ ")"
    
instance (Monoid r, Monad m) => Monoid (ByteString m r) where
  mempty = Empty mempty
  {-# INLINE mempty #-}
  mappend = liftM2 mappend
  {-# INLINE mappend #-}

instance (MonadBase b m) => MonadBase b (ByteString m) where
  liftBase  = mwrap . fmap return . liftBase
  {-#INLINE liftBase #-}

instance (MonadThrow m) => MonadThrow (ByteString m) where
  throwM = lift . throwM 
  {-#INLINE throwM #-}

instance (MonadCatch m) => MonadCatch (ByteString m) where
  catch str f = go str
    where
    go p = case p of
      Chunk bs rest  -> Chunk bs (go rest)
      Empty  r       -> Empty r
      Go  m          -> Go (catch (do
          p' <- m
          return (go p'))  
       (\e -> return (f e)) )
  {-#INLINABLE catch #-}
     
instance (MonadResource m) => MonadResource (ByteString m) where
  liftResourceT = lift . liftResourceT
  {-#INLINE liftResourceT #-}

bracketByteString :: (MonadResource m) =>
       IO a -> (a -> IO ()) -> (a -> ByteString m b) -> ByteString m b
bracketByteString alloc free inside = do
        (key, seed) <- lift (allocate alloc free)
        clean key (inside seed)
  where
    clean key = loop where
      loop str = case str of 
        Empty r        -> Go (release key >> return (Empty r))
        Go m           -> Go (liftM loop m)
        Chunk bs rest  -> Chunk bs (loop rest)
{-#INLINABLE bracketByteString #-}


data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

-- -- ------------------------------------------------------------------------
--
-- | Smart constructor for 'Chunk'.
consChunk :: S.ByteString -> ByteString m r -> ByteString m r
consChunk c@(S.PS _ _ len) cs 
  | len == 0  = cs
  | otherwise = Chunk c cs
{-# INLINE consChunk #-}

-- | Yield-style smart constructor for 'Chunk'.
chunk :: S.ByteString -> ByteString m ()
chunk bs = consChunk bs (Empty ())
{-# INLINE chunk #-}


{- | Reconceive an effect that results in an effectful bytestring as an effectful bytestring. 
    Compare Streaming.mwrap. The closes equivalent of 
  
>>> Streaming.wrap :: f (Stream f m r) -> Stream f m r

    is here  @consChunk@. @mwrap@ is the smart constructor for the internal @Go@ constructor.
-}
mwrap :: m (ByteString m r) -> ByteString m r
mwrap = Go
{-# INLINE mwrap #-}

-- | Construct a succession of chunks from its Church encoding (compare @GHC.Exts.build@)
materialize :: (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)
            -> ByteString m r
materialize phi = phi Empty Chunk Go
{-#INLINE[0] materialize #-}

-- | Resolve a succession of chunks into its Church encoding; this is
-- not a safe operation; it is equivalent to exposing the constructors
dematerialize :: Monad m
              => ByteString m r
              -> (forall x . (r -> x) -> (S.ByteString -> x -> x) -> (m x -> x) -> x)
dematerialize x0 nil cons mwrap = loop SPEC x0
  where
  loop !_ x = case x of
     Empty r    -> nil r
     Chunk b bs -> cons b (loop SPEC bs )
     Go ms -> mwrap (liftM (loop SPEC) ms)
{-# INLINE [1] dematerialize #-}

{-# RULES
  "dematerialize/materialize" forall (phi :: forall b . (r -> b) -> (S.ByteString -> b -> b) -> (m b -> b)  -> b). dematerialize (materialize phi) = phi ;
  #-}
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
{-#INLINE defaultChunkSize #-}
-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024
{-#INLINE smallChunkSize #-}

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)
{-#INLINE chunkOverhead #-}
-- ------------------------------------------------------------------------
-- | Packing and unpacking from lists
-- packBytes' :: Monad m => [Word8] -> ByteString m ()
-- packBytes' cs0 =
--     packChunks 32 cs0
--   where
--     packChunks n cs = case S.packUptoLenBytes n cs of
--       (bs, [])  -> Chunk bs (Empty ())
--       (bs, cs') -> Chunk bs (packChunks (min (n * 2) BI.smallChunkSize) cs')
--     -- packUptoLenBytes :: Int -> [Word8] -> (ByteString, [Word8])
--     packUptoLenBytes len xs0 =
--         unsafeDupablePerformIO (createUptoN' len $ \p -> go p len xs0)
--       where
--         go !_ !n []     = return (len-n, [])
--         go !_ !0 xs     = return (len,   xs)
--         go !p !n (x:xs) = poke p x >> go (p `plusPtr` 1) (n-1) xs
--         createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (S.ByteString, a)
--         createUptoN' l f = do
--             fp <- S.mallocByteString l
--             (l', res) <- withForeignPtr fp $ \p -> f p
--             assert (l' <= l) $ return (S.PS fp 0 l', res)
-- {-#INLINABLE packBytes' #-}

packBytes :: Monad m => Stream (Of Word8) m r -> ByteString m r
packBytes cs0 = do 
  (bytes :> rest) <- lift $ SP.toList $ SP.splitAt 32 cs0
  case bytes of
    [] -> case rest of
      Return r -> Empty r
      Step as  -> packBytes (Step as)  -- these two pattern matches
      Effect m -> Go $ liftM packBytes m -- should be evaded.
    _  -> Chunk (S.packBytes bytes) (packBytes rest)
{-#INLINABLE packBytes #-}

packChars :: Monad m => Stream (Of Char) m r -> ByteString m r
packChars = packBytes . SP.map S.c2w
{-#INLINABLE packChars #-}

    

unpackBytes :: Monad m => ByteString m r ->  Stream (Of Word8) m r
unpackBytes bss = dematerialize bss
    Return
    unpackAppendBytesLazy
    Effect
  where
  unpackAppendBytesLazy :: S.ByteString -> Stream (Of Word8) m r -> Stream (Of Word8) m r
  unpackAppendBytesLazy (S.PS fp off len) xs
    | len <= 100 = unpackAppendBytesStrict (S.PS fp off len) xs
    | otherwise  = unpackAppendBytesStrict (S.PS fp off 100) remainder
    where
      remainder  = unpackAppendBytesLazy (S.PS fp (off+100) (len-100)) xs

  unpackAppendBytesStrict :: S.ByteString -> Stream (Of Word8) m r -> Stream (Of Word8) m r
  unpackAppendBytesStrict (S.PS fp off len) xs =
   inlinePerformIO $ withForeignPtr fp $ \base -> do
        loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
    where
      accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
      loop !sentinal !p acc
        | p == sentinal = return acc
          | otherwise     = do x <- peek p
                               loop sentinal (p `plusPtr` (-1)) (Step (x :> acc))
{-# INLINABLE unpackBytes #-}

-- copied from Data.ByteString.Unsafe for compatibility with older bytestring
unsafeLast :: S.ByteString -> Word8
unsafeLast (S.PS x s l) = 
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+l-1)
 where
      accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE unsafeLast #-}

-- copied from Data.ByteString.Unsafe for compatibility with older bytestring
unsafeInit :: S.ByteString -> S.ByteString
unsafeInit (S.PS ps s l) = S.PS ps s (l-1)
{-# INLINE unsafeInit #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- | Consume the chunks of an effectful ByteString with a natural right fold.
foldrChunks :: Monad m => (S.ByteString -> a -> a) -> a -> ByteString m r -> m a
foldrChunks step nil bs = dematerialize bs
  (\_ -> return nil)
  (liftM . step)
  join
{-# INLINE foldrChunks #-}

foldlChunks :: Monad m => (a -> S.ByteString -> a) -> a -> ByteString m r -> m (Of a r)
foldlChunks f z = go z
  where go a _ | a `seq` False = undefined
        go a (Empty r)    = return (a :> r)
        go a (Chunk c cs) = go (f a c) cs
        go a (Go m)       = m >>= go a
{-# INLINABLE foldlChunks #-}

chunkMap :: Monad m => (S.ByteString -> S.ByteString) -> ByteString m r -> ByteString m r 
chunkMap f bs = dematerialize bs return (\bs bss -> Chunk (f bs) bss) Go
{-#INLINE chunkMap #-}

chunkMapM :: Monad m => (S.ByteString -> m S.ByteString) -> ByteString m r -> ByteString m r 
chunkMapM f bs = dematerialize bs return (\bs bss -> Go (liftM (flip Chunk bss) (f bs))) Go
{-#INLINE chunkMapM #-}

chunkMapM_ :: Monad m => (S.ByteString -> m x) -> ByteString m r -> m r 
chunkMapM_ f bs = dematerialize bs return (\bs mr -> f bs >> mr) join
{-#INLINE chunkMapM_ #-}


{- | @chunkFold@ is preferable to @foldlChunks@ since it is 
     an appropriate argument for @Control.Foldl.purely@ which 
     permits many folds and sinks to be run simulaneously on one bytestream.

  -}
chunkFold :: Monad m => (x -> S.ByteString -> x) -> x -> (x -> a) -> ByteString m r -> m (Of a r)
chunkFold step begin done = go begin
  where go a _ | a `seq` False = undefined
        go a (Empty r)    = return (done a :> r)
        go a (Chunk c cs) = go (step a c) cs
        go a (Go m)       = m >>= go a
{-# INLINABLE chunkFold #-}

{- | @chunkFoldM@ is preferable to @foldlChunksM@ since it is 
     an appropriate argument for @Control.Foldl.impurely@ which
     permits many folds and sinks to be run simulaneously on one bytestream.

  -}
chunkFoldM :: Monad m => (x -> S.ByteString -> m x) -> m x -> (x -> m a) -> ByteString m r -> m (Of a r)
chunkFoldM step begin done bs = begin >>= go bs
  where 
    go str !x = case str of 
      Empty r    -> done x >>= \a -> return (a :> r)
      Chunk c cs -> step x c >>= go cs
      Go m       -> m >>= \str' -> go str' x
{-# INLINABLE chunkFoldM  #-}

foldlChunksM :: Monad m => (a -> S.ByteString -> m a) -> m a -> ByteString m r -> m (Of a r)
foldlChunksM f z bs = z >>= \a -> go a bs
  where 
    go !a str = case str of 
      Empty r    -> return (a :> r)
      Chunk c cs -> f a c >>= \aa -> go aa cs
      Go m       -> m >>= go a 
{-# INLINABLE foldlChunksM #-}



-- | Consume the chunks of an effectful ByteString with a natural right monadic fold.
foldrChunksM :: Monad m => (S.ByteString -> m a -> m a) -> m a -> ByteString m r -> m a
foldrChunksM step nil bs = dematerialize bs
  (\_ -> nil)
  step
  join
{-# INLINE foldrChunksM #-}

unfoldrNE :: Int -> (a -> Either r (Word8, a)) -> a -> (S.ByteString, Either r a)
unfoldrNE i f x0
    | i < 0     = (S.empty, Right x0)
    | otherwise = unsafePerformIO $ S.createAndTrim' i $ \p -> go p x0 0
  where
    go !p !x !n
      | n == i    = return (0, n, Right x)
      | otherwise = case f x of
                      Left r     -> return (0, n, Left r)
                      Right (w,x') -> do poke p w
                                         go (p `plusPtr` 1) x' (n+1)
{-# INLINE unfoldrNE #-}


unfoldMChunks :: Monad m => (s -> m (Maybe (S.ByteString, s))) -> s -> ByteString m ()
unfoldMChunks step = loop where
  loop s = Go $ do
    m <- step s
    case m of 
      Nothing -> return (Empty ())
      Just (bs,s') -> return $ Chunk bs (loop s')
{-# INLINABLE unfoldMChunks #-}

unfoldrChunks :: Monad m => (s -> m (Either r (S.ByteString, s))) -> s -> ByteString m r
unfoldrChunks step = loop where
  loop !s = Go $ do
    m <- step s
    case m of 
      Left r -> return (Empty r)
      Right (bs,s') -> return $ Chunk bs (loop s')
{-# INLINABLE unfoldrChunks #-}


{-| Stream chunks from something that contains @IO (Maybe ByteString)@
    until it returns @Nothing@. @reread@ is of particular use rendering @io-streams@
    input streams as byte streams in the present sense

> Q.reread Streams.read             :: InputStream S.ByteString -> Q.ByteString IO ()
> Q.reread (liftIO . Streams.read)  :: MonadIO m => InputStream S.ByteString -> Q.ByteString m ()

The other direction here is 

> Streams.unfoldM Q.unconsChunk     :: Q.ByteString IO r -> IO (InputStream S.ByteString)

  -}
reread :: Monad m => (s -> m (Maybe S.ByteString)) -> s -> ByteString m ()
reread step s = loop where 
  loop = Go $ do 
    m <- step s
    case m of 
      Nothing -> return (Empty ())
      Just a  -> return (Chunk a loop)
{-# INLINEABLE reread #-}

{-| Make the information in a bytestring available to more than one eliminating fold, e.g.

>>>  Q.count 'l' $ Q.count 'o' $ Q.copy $ "hello\nworld"
3 :> (2 :> ())

>>> Q.length $ Q.count 'l' $ Q.count 'o' $ Q.copy $ Q.copy "hello\nworld"
11 :> (3 :> (2 :> ()))

>>> runResourceT $ Q.writeFile "hello2.txt" $ Q.writeFile "hello1.txt" $ Q.copy $ "hello\nworld\n"
>>> :! cat hello2.txt
hello
world
>>> :! cat hello1.txt
hello
world

    This sort of manipulation could as well be acheived by combining folds - using
    @Control.Foldl@ for example. But any sort of manipulation can be involved in
    the fold.  Here are a couple of trivial complications involving splitting by lines:

>>> let doubleLines = Q.unlines . maps (<* Q.chunk "\n" ) . Q.lines
>>> let emphasize = Q.unlines . maps (<* Q.chunk "!" ) . Q.lines
>>> runResourceT $ Q.writeFile "hello2.txt" $ emphasize $ Q.writeFile "hello1.txt" $ doubleLines $ Q.copy $ "hello\nworld"
>>> :! cat hello2.txt
hello!
world!
>>> :! cat hello1.txt
hello

world

    As with the parallel operations in @Streaming.Prelude@, we have

> Q.effects . Q.copy       = id
> hoist Q.effects . Q.copy = id

   The duplication does not by itself involve the copying of bytestring chunks;
   it just makes two references to each chunk as it arises. This does, however
   double the number of constructors associated with each chunk.

-}

copy
  :: Monad m =>
     ByteString m r -> ByteString (ByteString m) r
copy = loop where
  loop str = case str of
    Empty r         -> Empty r
    Go m            -> Go (liftM loop (lift m))
    Chunk bs rest   -> Chunk bs (Go (Chunk bs (Empty (loop rest))))

{-# INLINABLE copy #-}
