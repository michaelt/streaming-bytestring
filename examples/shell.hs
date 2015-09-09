-- these examples are from the io-streams tutorial
{-#LANGUAGE OverloadedStrings #-}
import Streaming
import Streaming.Prelude (yield, next, each)
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming.Char8 as Q
import System.IO (withFile, IOMode(..))
import Control.Monad
import Control.Applicative
import Data.Monoid

cat :: FilePath -> IO ()
cat file = withFile file ReadMode $ \h -> Q.stdout (Q.fromHandle h)
-- catStdin :: IO ()
-- catStdin = Q.stdout Q.stdin
       
head :: Int -> FilePath -> IO ()
head n file = withFile file ReadMode $ \h -> 
  Q.stdout         -- IO ()                      -- stream to IO.stdout
  $ Q.unlines      -- ByteString m ()            -- insert '\n' between bytestream layers
  $ takes n        -- Stream (ByteString m) m () -- nb. "takes" is 'functor general'
  $ Q.lines        -- Stream (ByteString m) m () -- divided into Stream layers
  $ Q.fromHandle h -- ByteString m ()            -- raw bytes

yes :: IO ()
yes =  Q.stdout $ Q.cycle "y\n" -- uses OverloadedStrings instance for 'ByteString m ()'

grep :: B.ByteString -> FilePath -> IO ()
grep pat file = withFile file ReadMode $ \h -> do
    let raw :: ByteString IO ()                            -- get raw bytes
        raw = Q.fromHandle h 
        
        segmented :: Stream (ByteString IO) IO ()          -- divide on newlines
        segmented = Q.lines raw
        
        individualized :: Stream (Of B.ByteString) IO ()
        individualized = mapsM Q.toStrict' segmented       -- danger: concatenate 'real' bytestrings!
        
        matching :: Stream (Of B.ByteString) IO ()         -- filter out matching bytestrings
        matching = S.filter (B.isInfixOf pat) individualized 
        
        deindividualized :: Stream (ByteString IO) IO ()   -- restream (implicitly using
        deindividualized  = for matching (layer . Q.chunk) --     the new chunk structure)
        
        unsegmented :: ByteString IO ()                    -- add newlines
        unsegmented = Q.unlines deindividualized
        
    Q.stdout unsegmented                                   -- stream to IO.stdout


data Option = Bytes | Words | Lines
len = S.fold (\n _ -> n + 1) 0 id
             
wc :: Option -> FilePath -> IO ()
wc opt file = withFile file ReadMode $ \h -> 
    do n <- count (Q.fromHandle h) 
       print n
  where
    count is = case opt of
        Bytes -> Q.length is
        Words -> S.sum $ mapsM blank_layer $ Q.words is
        Lines -> S.sum $ mapsM blank_layer $ Q.lines is
    blank_layer :: Monad m => ByteString m r -> m (Of Int r)
    blank_layer = liftM (1 :>) . Q.drain 
    -- replace each layer with (1 :> ...);  here we do not accumlate strict-bs words

-- exercise: write `wc` to permit a list of options, using `foldl` to combine
-- the different folds.  This would require a more direct implementation
-- of what might be called `line_count :: Fold Char Int` and `word_count :: Fold Char Int`

paste file file' = withFile file ReadMode $ \h -> 
                   withFile file' ReadMode $ \h' ->
  Q.stdout
  $ Q.unlines
  $ interleaves (Q.lines (Q.fromHandle h)) 
  $ maps (Q.cons '\t') 
  $ Q.lines 
  $ Q.fromHandle h'

nl :: FilePath -> IO () 
nl file = withFile file ReadMode $ \h -> 
    Q.stdout
    $ Q.unlinesIndividual   
    $ S.zipWith nlpad (each [1..])
    $ Q.linesIndividual    -- note unnecessary failure to stream; see below
    $ Q.fromHandle h       
 where
  nlpad :: Int -> B.ByteString -> B.ByteString
  nlpad n bs = padding <> B.pack (show n <> " ") <> bs 
   where
     len = length (show n); diff = 9 - len
     padding = if diff > 0  then B.replicate diff ' ' else B.singleton ' '

nl_streaming :: FilePath -> IO () 
nl_streaming file = withFile file ReadMode $ \h -> 
   Q.stdout
   $ Q.unlines
   $ interleaves numerals -- interleaves might instead be called 'fuseLayers' or something
   $ Q.lines              -- note proper streaming    
   $ Q.fromHandle h
  where 
  numerals :: Monad m => Stream (ByteString m) m ()
  numerals = maps trans (each [1..]) where
    trans (n:>r) = Q.chunk stuff >> return r
     where
        len = length (show n); diff = 9 - len
        padding = if diff > 0  then B.replicate diff ' ' else B.singleton ' '
        stuff = padding <> B.pack (show n ++ " ")