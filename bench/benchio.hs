{-#LANGUAGE OverloadedStrings #-}
import qualified System.IO as IO
import qualified System.Environment as IO
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Streaming.Char8 as S
import Data.ByteString.Char8 (pack)
import qualified Pipes.Group as PG
import qualified Pipes.ByteString as PB
import qualified Data.Conduit.Binary as CB
import Pipes
import Data.Conduit
import Data.Monoid
import Lens.Simple
import Data.Char
main = 
  IO.withFile "txt/words3b.txt" IO.ReadMode  $ \hIn  ->
  IO.withFile "txt/words3c.txt" IO.WriteMode $ \hOut -> 
  do xs <- IO.getArgs
     case xs of 
       x:_ -> 
         case x of
           "lazy"      -> L.hGetContents hIn >>= L.hPut hOut
           "streaming" -> S.hPut hOut (S.hGetContents hIn) 
           "conduit"   -> CB.sourceHandle hIn $$ CB.sinkHandle hOut       
           "pipes"     -> runEffect $ PB.fromHandle hIn >-> PB.toHandle hOut
           _           -> info
       []              -> info
 where 
   info = putStrLn "lazy conduit pipe streaming"
  
