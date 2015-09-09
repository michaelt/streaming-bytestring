{-#LANGUAGE OverloadedStrings #-}
import Streaming
import Streaming.Prelude (each, next, yield)
import qualified Data.ByteString.Streaming.Char8 as Q
import qualified Data.ByteString.Char8 as B
import qualified Streaming.Prelude as S
import qualified Control.Foldl as L
import Data.ByteString.Streaming.HTTP -- git clone https://github.com/michaelt/streaming-http
                                      -- cabal install ./streaming-http
infixl 5 >>>; (>>>) = flip (.)
infixl 1 &; (&) = flip ($)

main = do
    req <- parseUrl "https://raw.githubusercontent.com/michaelt/kjv/master/kjv.txt"
    m <- newManager tlsManagerSettings 
    withHTTP req m $ \resp -> 
      resp &                   -- Response (Q.ByteString IO ())
      responseBody             -- Q.ByteString IO ()
      >>> Q.lines              -- Stream (Q.ByteString IO) IO ()
      >>> Q.denull             -- Stream (Q.ByteString IO) IO ()
      >>> interleaves numerals -- Stream (Q.ByteString IO) IO ()
      >>> Q.unlines            -- Q.ByteString IO ()
      >>> Q.stdout             -- IO ()
 where
  numerals = maps padded (S.enumFrom 1)
  padded (n:>r) = Q.chunk stuff >> return r
       where
          len = length (show n); diff = 5 - len
          padding = if diff > 0  then B.replicate diff ' ' else ""
          stuff = padding `mappend` B.pack (show n ++ " ")


  