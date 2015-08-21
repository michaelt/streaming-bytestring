{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}
-- This is a very close replica of Renzo C's `pipes-aeson`

-- | This module provides internal utilities and it is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Aeson" module instead.
module Data.ByteString.Streaming.Aeson
  ( DecodingError(..)
  , encode
  , decode
  , decoded
  ) where
import           Control.Exception                (Exception)
import           Control.Monad.Trans
import qualified Control.Monad.Trans.State.Strict as S
import           Control.Monad.Trans.State.Strict (StateT(..))

import qualified Data.Aeson                       as Ae
import qualified Data.Attoparsec.ByteString       as Attoparsec
import qualified Data.ByteString                  as S
import qualified Data.ByteString.Internal         as S (isSpaceWord8)
import           Data.Data                        (Data, Typeable)
-- import           Pipes
import qualified Data.Attoparsec.ByteString.Streaming  as PA
import           Data.ByteString.Streaming 
import Data.ByteString.Streaming.Internal
import qualified Data.ByteString.Streaming as B
import Streaming
import Streaming.Internal (Stream(..))


--------------------------------------------------------------------------------
type Parser m a = forall x . StateT (ByteString m x) m a
-- | An error while decoding a JSON value.

type ParsingError = ([String],String)

data DecodingError
  = AttoparsecError ParsingError
    -- ^An @attoparsec@ error that happened while parsing the raw JSON string.
  | FromJSONError String
    -- ^An @aeson@ error that happened while trying to convert a
    -- 'Data.Aeson.Value' to an 'A.FromJSON' instance, as reported by
    -- 'Data.Aeson.Error'.
  deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError

-- | This instance allows using 'Pipes.Lift.errorP' with 'Pipes.Aeson.decoded'
-- and 'Pipes.Aeson.decodedL'
-- instance Error (DecodingError, Producer a m r)

--------------------------------------------------------------------------------

-- | Consecutively parse 'a' elements from the given 'Producer' using the given
-- parser (such as 'Pipes.Aeson.decode' or 'Pipes.Aeson.parseValue'), skipping
-- any leading whitespace each time.
--
-- This 'Producer' runs until it either runs out of input or until a decoding
-- failure occurs, in which case it returns 'Left' with a 'DecodingError' and
-- a 'Producer' with any leftovers. You can use 'Pipes.Lift.errorP' to turn the
-- 'Either' return value into an 'Control.Monad.Trans.Error.ErrorT'
-- monad transformer.


-- | Like 'Pipes.Aeson.encode', except it accepts any 'Ae.ToJSON' instance,
-- not just 'Ae.Array' or 'Ae.Object'.
encode :: (Monad m, Ae.ToJSON a) => a -> ByteString m ()
encode = fromLazy . Ae.encode

decode
  :: (Monad m, Ae.FromJSON a)
  => StateT (ByteString m x) m (Either DecodingError a)
decode = do
      mev <- PA.atto Ae.json'
      return $ case mev of
         Right l      -> Left (AttoparsecError l)
         Left v -> case Ae.fromJSON v of
            Ae.Error e   -> Left (FromJSONError e)
            Ae.Success a -> Right a

decoded  :: (Monad m, Ae.FromJSON a) =>
     ByteString m r
     -> Stream (Of a) m (Either (DecodingError, ByteString m r) r)
decoded = consecutively decode 
  where
  consecutively
    :: (Monad m)
    => StateT (ByteString m r) m (Either e a)
    -> ByteString m r  
    -> Stream (Of a) m (Either (e, ByteString m r) r)
  consecutively parser = step where
    step p0 = do
      x <- lift $ nextSkipBlank p0
      case x of
        Left r -> Return (Right r)
        Right (bs, p1) -> do
          (mea, p2) <- lift $ S.runStateT parser (Chunk bs p1)
          case mea of
            Right a -> Step (a :> step p2)
            Left  e -> Return (Left (e, p2))

  
  nextSkipBlank p0 = do
        x <- nextChunk p0
        case x of
           Left  _      -> return x
           Right (a,p1) -> do
              let a' = S.dropWhile S.isSpaceWord8 a
              if S.null a' then nextSkipBlank p1
                           else return (Right (a', p1))