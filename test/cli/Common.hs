{- © 2019 Serokell
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Common
  ( ShellTest (..)
  , shellTest

  , shell
  , shells
  , writeShellScript
  ) where

import Control.Applicative ((<*))
import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.IO as IO
import qualified System.Posix.Files as Posix
import qualified System.Process as P


data ShellTest m = ShellTest
  { setup :: m ()
  , teardown :: m ()
  , run :: Text
  }

shellTest :: ShellTest IO -> IO [Text]
shellTest ShellTest{setup, teardown, run} =
    withSystemTempDirectory "test-overcight" $ \tdir ->
    bracket
     (Dir.getCurrentDirectory <* Dir.setCurrentDirectory tdir)
     Dir.setCurrentDirectory
     $ \pwd -> do
      Env.setEnv "TEST_ROOT" pwd
      bracket
        setup
        (\() -> teardown)
        (\() -> (T.lines . T.pack) <$> P.readCreateProcess (P.shell cmd) "")
  where
    cmd = T.unpack run


shell :: MonadIO m => Text -> m ()
shell = liftIO . P.callCommand . T.unpack

shells :: MonadIO m => [Text] -> m ()
shells = traverse_ shell

writeShellScript :: MonadIO m => FilePath -> Text -> m ()
writeShellScript fp t = do
  liftIO $ IO.withFile fp IO.WriteMode $ \h -> do
    T.hPutStrLn h "#!/usr/bin/env bash"
    T.hPutStrLn h ""
    T.hPutStrLn h t
  liftIO $ Posix.setFileMode fp Posix.ownerExecuteMode
