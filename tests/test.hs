import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

import Control.Applicative
import Data.Functor.Identity
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Streaming.Char8 as SBS8
import Text.Printf
import qualified Streaming.Prelude as S

listOf :: Monad m => Series m a -> Series m [a]
listOf a = decDepth $
  pure [] \/ ((:) <$> a <~> listOf a)

strSeries :: Monad m => Series m String
strSeries = listOf (generate $ const ['a', 'b', '\n'])

chunksSeries :: Monad m => Series m [String]
chunksSeries = listOf strSeries

fromChunks :: [String] -> SBS8.ByteString Identity ()
fromChunks = SBS8.fromChunks . S.each .  map BS8.pack

unix2dos :: String -> String
unix2dos = concatMap $ \c -> if c == '\n' then "\r\n" else [c]

s_lines :: SBS8.ByteString Identity () -> [BS8.ByteString]
s_lines
  = runIdentity
  . S.toList_
  . S.mapped SBS8.toStrict
  . SBS8.lines

main = defaultMain $ testGroup "Tests"
  [ testGroup "lines" $
    [ testProperty "Data.ByteString.Streaming.Char8.lines is equivalent to Prelude.lines" $ over chunksSeries $ \chunks ->
        -- This only makes sure that the streaming-bytestring lines function
        -- matches the Prelude lines function when no carriage returns
        -- are present. They are not expected to have the same behavior
        -- with dos-style line termination.
        let expected = lines $ concat chunks
            got = (map BS8.unpack . s_lines . fromChunks) chunks
        in
        if expected == got
          then Right ""
          else Left (printf "Expected %s; got %s" (show expected) (show got) :: String)
    , testProperty "lines recognizes DOS line endings" $ over strSeries $ \str ->
        s_lines (SBS8.string $ unix2dos str) == s_lines (SBS8.string str)
    , testProperty "lines recognizes DOS line endings with tiny chunks" $ over strSeries $ \str ->
        s_lines (mapM_ SBS8.singleton $ unix2dos str) == s_lines (mapM_ SBS8.singleton str)
    ]
  ]
