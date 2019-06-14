{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE BangPatterns #-}

module RioState
  ( evalState
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (askUnliftIO), UnliftIO (UnliftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)


newtype RioState s a = RioState (ReaderT (IORef s) IO a)
  deriving (Functor, Applicative, Monad)

instance MonadIO (RioState s) where
  liftIO = RioState . liftIO

instance MonadUnliftIO (RioState s) where
  askUnliftIO = RioState $ do
    sref <- ask
    pure (UnliftIO $ \(RioState r) -> runReaderT r sref)

instance MonadState s (RioState s) where
  state f = RioState $ do
    sref <- ask
    s <- liftIO $ readIORef sref
    let (!a, !s') = f s
    liftIO $ writeIORef sref s'
    pure a

evalState :: RioState s a -> s -> IO a
evalState (RioState r) s0 = do
  sref <- newIORef s0
  runReaderT r sref
