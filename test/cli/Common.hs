module Common
  ( ShellTest (..)
  , shellTest

  , shell
  , shells
  , writeShellScript
  ) where

import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import qualified System.Process as P


data ShellTest m = ShellTest
  { setup :: m ()
  , teardown :: m ()
  , run :: Text
  }

shellTest :: ShellTest IO -> IO [Text]
shellTest ShellTest{setup, teardown, run} =
    bracket setup (\() -> teardown) $ \() ->
      withSystemTempDirectory "test-overcight" $ \tdir ->
        (T.lines . T.pack) <$> P.readCreateProcess (P.shell $ cmd tdir) ""
  where
    cmd tdir =
      "export TEST_ROOT=$(pwd)/test/cli && cd \"" <> tdir <> "\" && " <> T.unpack run


shell :: MonadIO m => Text -> m ()
shell = liftIO . P.callCommand . T.unpack

shells :: MonadIO m => [Text] -> m ()
shells = traverse_ shell

writeShellScript :: MonadIO m => FilePath -> Text -> m ()
writeShellScript fp t =
  liftIO $ IO.withFile fp IO.WriteMode $ \h -> do
    T.hPutStrLn h "#!/usr/bin/env bash"
    T.hPutStrLn h ""
    T.hPutStrLn h t
