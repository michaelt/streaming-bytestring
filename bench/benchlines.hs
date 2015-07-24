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
import qualified Data.Conduit.List as CL
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
           "lazy"      -> L.hGetContents hIn >>= L.hPut hOut . l
           "streaming" -> S.hPut hOut (s (S.hGetContents hIn) )
           "conduit"   -> CB.sourceHandle hIn $$ CB.sinkHandle hOut       
           "pipes"     -> runEffect $ p (PB.fromHandle hIn) >-> PB.toHandle hOut
           _           -> info
       []              -> info
 where 
   info = putStrLn "lazy conduit pipe streaming"
  

s x = S.unlines (S.maps ( S.yield "!" >>)  (S.lines x))
l x = LC.unlines (map ("!" <>)  (LC.lines x))
p = over PB.lines (PG.maps (Pipes.yield "!" >>))
c = CB.lines =$= go 
 where
   go = do 
     m <- await
     case m of 
       Nothing -> return ()
       Just x -> do 
         yield "\n"
         yield x
         go
-- ss  = S.unlines . S.zipWithList S.Chunk titles . S.lines
-- ll = LC.unlines . zipWith (\x y -> LC.fromStrict x <> y) titles . LC.lines

titles = map title [1..] where
  title n = 
    let str = show n
        len = length str
        pad = take (10-len) (repeat ' ')
    in pack (str <> pad)
--
-- s' = S.map toLower . S.intercalate  (S.yield "!\n") . S.lines
-- l' = LC.map toLower . LC.intercalate "!\n"  . LC.lines
