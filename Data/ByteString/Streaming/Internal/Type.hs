{-# LANGUAGE TypeFamilies#-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE FlexibleContexts#-}
module Data.ByteString.Streaming.Internal.Type where

import Control.Monad (ap,liftM, join)
import Control.Monad.Morph
import Control.Monad.Trans
import qualified Data.Foldable as F
import qualified Control.Foldl as L

data List f m r = Return r | Step !(f (List f m r)) | Wrap (m (List f m r))

instance (Functor f, Monad m) => Functor (List f m) where
  fmap f = \case
    Return r -> Return (f r)
    Step fls -> Step (fmap (fmap f) fls)
    Wrap mls -> Wrap (liftM (fmap f) mls)

instance (Functor f, Monad m) =>  Monad (List f m) where
  return = Return
  a >>= f = case a of
    Return r    -> f r
    Step fls -> Step (fmap (>>= f) fls)
    Wrap mls -> Wrap (liftM (>>= f) mls)

instance (Functor f, Monad m) => Applicative (List f m) where
  pure = Return
  (<*>) = ap

instance (Functor f) => MFunctor (List f) where
  hoist phi = \case
    Return r -> Return r
    Step fls -> Step (fmap (hoist phi) fls)
    Wrap mls -> Wrap (phi (liftM (hoist phi) mls))

instance MonadTrans (List f) where
  lift ma = Wrap $ liftM Return ma

-- this could take Functor f instead, with alterations; which is best?
maps :: (Functor g, Monad m) => (forall x. f x -> g x) -> List f m r -> List g m r
maps phi = \case Return r -> Return r
                 Step fls -> Step (fmap (maps phi) (phi fls))
                 Wrap mls -> Wrap (liftM (maps phi) mls)

mapsM :: (Functor g, Monad m) => (forall x. f x -> m (g x)) -> List f m r -> List g m r
mapsM phi = \case Return r -> Return r
                  Step fls -> Wrap $ liftM (Step . fmap (mapsM phi))  (phi fls)
                  Wrap mls -> Wrap (liftM (mapsM phi) mls)

type List_ f m r = forall x . (r -> x) -> (f x -> x) -> (m x -> x) ->  x

blank = Return ()
{-#INLINE blank #-}

-- build, fold
construct :: (forall x . (r -> x) -> (f x -> x) -> (m x -> x) ->  x)
          -> List f m r
construct psi = psi Return Step Wrap

destroy :: (Functor f, Monad m)
        => List f m r
        -> (forall x . (r -> x) -> (f x -> x) -> (m x -> x) ->  x)
destroy = \lst nil cons wrap ->
  let loop = \case Step fls -> cons (fmap loop fls)
                   Wrap mls -> wrap (liftM loop mls)
                   Return r -> nil r
  in  loop lst

unfold
  :: (Functor f, Monad m)
  => (s -> m (Either r (f s)))
  -> s
  -> List f m r
unfold coalg begin = loop begin where
  loop s = Wrap $ do
    e <- coalg s
    case e of
      Left r  -> return $ Return r
      Right f -> return $ Step (fmap loop f)

unfoldM :: (Functor f, Monad m)
      => (a -> m (Either r (f a))) -> a -> List f m r
unfoldM f = let loop = Wrap . liftM (either Return (Step . fmap loop)) . f
            in loop

uncons ::  (Functor f, Monad m) => List f m r -> m (Either r (f (List f m r )))
uncons p = case p of
  Return r -> return (Left r)
  Step fls -> return (Right fls)
  Wrap mls -> mls >>= uncons

-- other preludish functions that are list/free general

takes :: (Functor f, Monad m) => Int -> List f m r -> List f m ()
takes = loop where
  loop 0 ls = Return ()
  loop n ls = case ls of
    Return _ -> Return ()
    Step fls -> Step (fmap (loop (n-1)) fls)
    Wrap mls -> Wrap (liftM (loop n) mls)

splitsAt :: (Functor f, Monad m)
         => Int
         -> List f m r
         -> List f m (List f m r)
splitsAt = loop where
  loop 0 ls = Return ls
  loop n (Return r) = Return (Return r)
  loop n (Step fls) = Step (fmap (loop (n-1)) fls)

distribute
  :: (Functor f, Monad m, MonadTrans t, MFunctor t,
      Monad (t m), Monad (t (List f m)))
  => List f (t m) a
  -> t (List f m) a
distribute ls = destroy ls
                   return
                   (join . lift . Step . fmap Return )
                   (join . hoist (Wrap . fmap Return))

listZipWith
  :: (Monad m, Functor g)
  =>  (forall x . a -> f x -> g x)
  -> [a]
  -> List f m r
  -> List g m r
listZipWith op zs = loop zs
  where
    loop [] ls      = loop zs ls
    loop (x:xs)  ls = case ls of
      Return r -> Return r
      Step fls -> Step $ fmap (loop xs) (op x fls)
      Wrap mls -> Wrap $ liftM (loop (x:xs)) mls
  
data Of a b = Of !a b deriving (Show, Eq, Ord, Read)
instance Functor (Of a) where fmap f (Of a b) = Of a (f b)

type Producer a m r = List (Of a) m r

yield a = Step (Of a (Return ()))

for :: (Monad m, Functor f) => List (Of a) m r -> (a -> List f m x) -> List f m r
for ls act = loop ls where
  loop ls0 = case ls0 of 
    Return r       -> Return r
    Step (Of a ls) -> act a >> loop ls
    Wrap mls       -> Wrap (fmap loop mls)


forM_ :: Monad m => List (Of a) m r -> (a -> m x) -> m r
forM_ ls act = loop ls where
  loop ls0 = case ls0 of
    Return r       -> return r
    Step (Of a ls) -> act a >> loop ls
    Wrap mls       -> mls >>= loop





    