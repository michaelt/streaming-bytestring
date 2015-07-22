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
      Result(..)
      , atto
      , atto_
    , module Data.Attoparsec.ByteString
    -- * Running parsers
    , parse
    , parseTest
    -- ** Result conversion
    , maybeResult
    , eitherResult
    )
    where

import Control.DeepSeq (NFData(rnf))
-- import Data.ByteString.Lazy.Internal (ByteString(..), chunk)
import qualified Data.List as L (intercalate)
import qualified Data.ByteString as B
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString
    hiding (IResult(..), Result, eitherResult, maybeResult,
            parse, parseWith, parseTest)
import Data.ByteString.Streaming.Internal.Type
import Data.ByteString.Streaming
-- | The result of a parse.



-- | Run a parser and return its result.
atto :: Monad m => A.Parser a -> StateT (ByteString m x) m (Either a ([String], String))
atto p = StateT loop where
  loop s  = case s of
              Chunk x xs -> go (A.parse p x) xs
              Empty r    -> go (A.parse p B.empty) (Empty r)
              Go m       -> m >>= loop 

  go (T.Fail x stk msg) ys      = return $ (Right (stk, msg), Chunk x ys) 
  go (T.Done x r) ys            = return $ (Left r, Chunk x ys) 
  go (T.Partial k) (Chunk y ys) = go (k y) ys
  go (T.Partial k) (Go m)       = m >>= go (T.Partial k) 
  go (T.Partial k) empty        = go (k B.empty) empty


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

data Result x m r = Fail (ByteString m x) [String] String
              -- ^ The parse failed.  The 'ByteString' is the input
              -- that had not yet been consumed when the failure
              -- occurred.  The @[@'String'@]@ is a list of contexts
              -- in which the error occurred.  The 'String' is the
              -- message describing the error, if any.
              | Done (ByteString m x) r
              -- ^ The parse succeeded.  The 'ByteString' is the
              -- input that had not yet been consumed (if any) when
              -- the parse succeeded.
              
instance Show r => Show (Result a m r) where
    show (Fail (Chunk bs p) stk msg) =
        "Fail  " ++ show bs ++ ", etc.:  " ++ show stk ++ " " ++ show msg
    show (Fail (Empty r) stk msg) =  
        "Fail, byte stream ended:  " ++ show stk ++ " " ++ show msg
    show (Fail (Go m) stk msg) =
        "Fail, bytestring in progress: " ++ show stk ++ " " ++ show msg
    show (Done bs r)       = "Done  " ++ show r

fmapR :: (a -> b) -> Result x m a -> Result x m b
fmapR _ (Fail st stk msg) = Fail st stk msg
fmapR f (Done bs r)       = Done bs (f r)

instance Functor (Result a m) where fmap = fmapR

-- | Run a parser and return its result.
parse :: Monad m => A.Parser a -> ByteString m x -> m (Result x m a)
parse p s = case s of
              Chunk x xs -> go (A.parse p x) xs
              Empty r    -> go (A.parse p B.empty) (Empty r)
              Go m       -> m >>= parse p
  where
    go (T.Fail x stk msg) ys      = return $ Fail (Chunk x ys) stk msg
    go (T.Done x r) ys            = return $ Done (Chunk x ys) r
    go (T.Partial k) (Chunk y ys) = go (k y) ys
    go (T.Partial k) (Go m)       = m >>= go (T.Partial k) 
    go (T.Partial k) empty        = go (k B.empty) empty


-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => A.Parser a -> ByteString IO r -> IO ()
parseTest p s = parse p s >>= print

-- | Convert a 'Result' value to a 'Maybe' value.
maybeResult :: Result a m r -> Maybe r
maybeResult (Done _ r) = Just r
maybeResult _          = Nothing

-- | Convert a 'Result' value to an 'Either' value.
eitherResult :: Result a m r -> Either String r
eitherResult (Done _ r)        = Right r
eitherResult (Fail _ [] msg)   = Left msg
eitherResult (Fail _ ctxs msg) = Left (L.intercalate " > " ctxs ++ ": " ++ msg)