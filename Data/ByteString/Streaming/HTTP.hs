-- | This module replicates `pipes-http` as closely as will type-check.
-- 
--   Here is an example GET request that streams the response body to standard
--   output:
--
-- > import qualified Data.ByteString.Streaming as S
-- > import Data.ByteString.Streaming.HTTP
-- >
-- > main = do
-- >   req <- parseUrl "https://www.example.com"
-- >   m <- newManager tlsManagerSettings 
-- >   withHTTP req m $ \resp -> S.stdout (responseBody resp) 
-- > 
--
--   Here is an example POST request that also streams the request body from
--   standard input:
--
-- > {-#LANGUAGE OverloadedStrings #-}
-- > import qualified Data.ByteString.Streaming as S
-- > import Data.ByteString.Streaming.HTTP
-- > 
-- > main = do
-- >    req <- parseUrl "https://www.example.com"
-- >    let req' = req
-- >            { method = "POST"
-- >            , requestBody = stream S.stdin
-- >            }
-- >    m <- newManager tlsManagerSettings
-- >    withHTTP req' m $ \resp -> S.stdout (responseBody resp)
--
-- For non-streaming request bodies, study the 'RequestBody' type, which also
-- accepts strict \/ lazy bytestrings or builders.


module Data.ByteString.Streaming.HTTP (
    -- * http-client
    -- $httpclient
      module Network.HTTP.Client
    , module Network.HTTP.Client.TLS

    -- * Streaming Interface
    , withHTTP
    , streamN
    , stream

    ) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.ByteString.Streaming
import Data.ByteString.Streaming.Internal
import Control.Monad.Trans

{- $httpclient
    This module is a thin @streaming-bytestring@ wrapper around the @http-client@ and
    @http-client-tls@ libraries.

    Read the documentation in the "Network.HTTP.Client" module of the
    @http-client@ library to learn about how to:

    * manage connections using connection pooling,

    * use more advanced request\/response features,

    * handle exceptions, and:
    
    * manage cookies.

    @http-client-tls@ provides support for TLS connections (i.e. HTTPS).
-}

-- | Send an HTTP 'Request' and wait for an HTTP 'Response'
withHTTP
    :: Request
    -- ^
    -> Manager
    -- ^
    -> (Response (ByteString IO ()) -> IO a)
    -- ^ Handler for response
    -> IO a
withHTTP r m k = withResponse r m k'
  where
    k' resp = do
        let p = (from . brRead . responseBody) resp
        k (resp { responseBody = p})
{-# INLINABLE withHTTP #-}

-- | Create a 'RequestBody' from a content length and an effectful 'ByteString'
streamN :: Int64 -> ByteString IO () -> RequestBody
streamN n p = RequestBodyStream n (to p)
{-# INLINABLE streamN #-}

{-| Create a 'RequestBody' from an effectful 'ByteString'

    'stream' is more flexible than 'streamN', but requires the server to support
    chunked transfer encoding.
-}
stream :: ByteString IO () -> RequestBody
stream p = RequestBodyStreamChunked (to p)
{-# INLINABLE stream #-}

to :: ByteString IO () -> (IO B.ByteString -> IO ()) -> IO ()
to p0 k = do
    ioref <- newIORef p0
    let readAction :: IO B.ByteString
        readAction = do
            p <- readIORef ioref
            case p of
                Empty   ()      -> do
                    writeIORef ioref (return ())
                    return B.empty
                Go m -> do 
                  p' <- m
                  writeIORef ioref p'
                  readAction
                Chunk bs p' -> do
                    writeIORef ioref p'
                    return bs
    k readAction 

from :: IO B.ByteString -> ByteString IO ()
from io = go
  where
    go = do
        bs <- lift io
        unless (B.null bs) $ do
            chunk bs
            go 