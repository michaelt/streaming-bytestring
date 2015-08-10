{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-} -- Imports internal modules

-- |
-- Module      :  Data.Attoparsec.ByteString.Lazy
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing that can consume lazy
-- 'ByteString' strings, loosely based on the Parsec library.
--
-- This is essentially the same code as in the 'Data.Attoparsec'
-- module, only with a 'parse' function that can consume a lazy
-- 'ByteString' incrementally, and a 'Result' type that does not allow
-- more input to be fed in.  Think of this as suitable for use with a
-- lazily read file, e.g. via 'L.readFile' or 'L.hGetContents'.
--
-- /Note:/ The various parser functions and combinators such as
-- 'string' still expect /strict/ 'B.ByteString' parameters, and
-- return strict 'B.ByteString' results.  Behind the scenes, strict
-- 'B.ByteString' values are still used internally to store parser
-- input and manipulate it efficiently.

module Data.ByteString.Streaming.Attoparsec
    (
      parse
      , parsed
      , atto
      , atto_
    , module Data.Attoparsec.ByteString

    )
    where

import Control.DeepSeq (NFData(rnf))
import qualified Data.ByteString as B
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString
    hiding (IResult(..), Result, eitherResult, maybeResult,
            parse, parseWith, parseTest)
import Streaming.Internal
import Data.ByteString.Streaming


-- | The result of a parse.

parse :: Monad m 
      => A.Parser a 
      -> ByteString m x 
      -> m (Either a ([String], String), ByteString m x)
parse p s  = case s of
    Chunk x xs -> go (A.parse p x) xs
    Empty r    -> go (A.parse p B.empty) (Empty r)
    Go m       -> m >>= parse p
  where
  go (T.Fail x stk msg) ys      = return $ (Right (stk, msg), Chunk x ys)
  go (T.Done x r) ys            = return $ (Left r, Chunk x ys)
  go (T.Partial k) (Chunk y ys) = go (k y) ys
  go (T.Partial k) (Go m)       = m >>= go (T.Partial k)
  go (T.Partial k) empty        = go (k B.empty) empty


-- | Run a parser and return its result.
atto :: Monad m => A.Parser a -> StateT (ByteString m x) m (Either a ([String], String))
atto p = StateT (parse p)

atto_ :: Monad m => A.Parser a -> ExceptT ([String], String) (StateT (ByteString m x) m) a
atto_ p = ExceptT $ StateT loop where
  loop s  = case s of
      Chunk x xs -> go (A.parse p x) xs
      Empty r    -> go (A.parse p B.empty) (Empty r)
      Go m       -> m >>= loop

  go (T.Fail x stk msg) ys      = return $ (Left (stk, msg), Chunk x ys)
  go (T.Done x r) ys            = return $ (Right r, Chunk x ys)
  go (T.Partial k) (Chunk y ys) = go (k y) ys
  go (T.Partial k) (Go m)       = m >>= go (T.Partial k)
  go (T.Partial k) empty        = go (k B.empty) empty


parsed
  :: Monad m
  => A.Parser a  -- ^ Attoparsec parser
  -> ByteString m r         -- ^ Raw input
  -> Stream (Of a) m (Either (([String],String), ByteString m r) r)
parsed parser = go
  where
    go p0 = do
      x <- lift (nextChunk p0)
      case x of
        Left r       -> Return (Right r)
        Right (bs,p1) -> step (yield bs >>) (A.parse parser bs) p1
    step diffP res p0 = case res of
      A.Fail _ c m -> Return (Left ((c,m), diffP p0))
      A.Done bs b  -> Step (b :> go (yield bs >> p0))
      A.Partial k  -> do
        x <- lift (nextChunk p0)
        case x of
          Left e -> step diffP (k mempty) (return e)
          Right (a,p1) -> step (diffP . (yield a >>)) (k a) p1
{-# INLINABLE parsed #-}