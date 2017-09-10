import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

import Control.Applicative
import Data.Functor.Identity
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Streaming.Char8 as SBS8
import qualified Data.ByteString.Streaming.Internal as SBSI
import Text.Printf
import qualified Streaming.Prelude as S
import qualified Streaming as SM
import Data.String (fromString)
import Data.Functor.Identity
import qualified Data.List as L

listOf :: Monad m => Series m a -> Series m [a]
listOf a = decDepth $
  pure [] \/ ((:) <$> a <~> listOf a)

strSeries :: Monad m => Series m String
strSeries = listOf (generate $ const ['a', 'b', '\n'])

strSeriesCrlf :: Monad m => Series m String
strSeriesCrlf = fmap L.concat $ listOf (generate $ const ["a", "b", "\r\n"])

chunksSeries :: Monad m => Series m [String]
chunksSeries = listOf strSeries

nats :: Monad m => Series m Int
nats = generate $ \d -> [1..d]

fromChunks :: [String] -> SBS8.ByteString Identity ()
fromChunks = SBS8.fromChunks . S.each .  map BS8.pack

unix2dos :: String -> String
unix2dos = concatMap $ \c -> if c == '\n' then "\r\n" else [c]

unpackToString :: SBS8.ByteString Identity () -> String
unpackToString = runIdentity . S.toList_ . SBS8.unpack

s_lines :: SBS8.ByteString Identity () -> [BS8.ByteString]
s_lines
  = runIdentity
  . S.toList_
  . S.mapped SBS8.toStrict
  . SBS8.lines

noNullChunks :: S.Stream (SBS8.ByteString Identity) Identity () -> Bool
noNullChunks = SM.streamFold (\() -> True) runIdentity go
  where
  go :: SBS8.ByteString Identity Bool -> Bool
  go (SBSI.Empty b) = b
  go (SBSI.Chunk bs sbs) = not (BS8.null bs) && go sbs
  go (SBSI.Go (Identity sbs)) = go sbs

main = defaultMain $ testGroup "Tests"
  [ testGroup "lines" $
    [ testProperty "Data.ByteString.Streaming.Char8.lines is equivalent to Prelude.lines" $ over chunksSeries $ \chunks ->
        let expected = lines $ concat chunks
            got = (map BS8.unpack . s_lines . fromChunks) chunks
        in
        if expected == got
          then Right ""
          else Left (printf "Expected %s; got %s" (show expected) (show got) :: String)
    , testProperty "lines recognizes DOS line endings" $ over strSeries $ \str ->
        s_lines (SBS8.string $ unix2dos str) == s_lines (SBS8.string str)
    , testProperty "lineSplit does not create null chunks (LF)" $ over ((,) <$> nats <~> strSeries) $ \(n,str) ->
        noNullChunks (SBS8.lineSplit n (fromString str))
    , testProperty "lineSplit does not create null chunks (CRLF)" $ over ((,) <$> nats <~> strSeriesCrlf) $ \(n,str) ->
        noNullChunks (SBS8.lineSplit n (fromString str))
    , testProperty "concat after lineSplit round trips (LF)" $ over ((,) <$> nats <~> strSeries) $ \(n,str) ->
        unpackToString (SBS8.concat (SBS8.lineSplit n (fromString str))) == str
    , testProperty "concat after lineSplit round trips (CRLF)" $ over ((,) <$> nats <~> strSeriesCrlf) $ \(n,str) ->
        unpackToString (SBS8.concat (SBS8.lineSplit n (fromString str))) == str
    ]
  ]
