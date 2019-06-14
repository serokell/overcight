{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Overc.Internal
  ( asCight
  , filterSuccess

  , cights
  ) where

import Prelude hiding (lookup)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, gets)
import Data.Conduit (ConduitT, (.|))
import System.Hatrace (TraceEvent, DetailedSyscallExit (..))
import System.Posix.Types (CPid)

import qualified Data.Conduit.List as CL
import qualified System.Hatrace as HT

import Overc.Cight (Cight (CightExec, CightRead), mkCightPath)
import Overc.Internal.FdState (FdState, lookup, updateFdState)


asCight ::
     MonadState FdState m
  => (CPid, DetailedSyscallExit)
  -> m (Maybe Cight)
asCight (pid, DetailedSyscallExit_read det) = do
  let fd = HT.fd (HT.enterDetail (det :: HT.SyscallExitDetails_read) :: HT.SyscallEnterDetails_read)
  gets (lookup (pid, fd)) >>= \case
    Nothing -> pure Nothing
    Just fp -> pure (Just $ CightRead $ mkCightPath fp)
asCight (pid, DetailedSyscallExit_write det) = do
  let fd = HT.fd (HT.enterDetail (det :: HT.SyscallExitDetails_write) :: HT.SyscallEnterDetails_write)
  gets (lookup (pid, fd)) >>= \case
    Nothing -> pure Nothing
    Just fp -> pure (Just $ CightRead $ mkCightPath fp)
-- TODO: execve should be processed on enter
-- asCight (pid, DetailedSyscallExit_execve det) = _
asCight _ = pure Nothing

filterSuccess ::
     Monad m
  => ConduitT
      (CPid, Either (HT.Syscall, HT.ERRNO) DetailedSyscallExit)
      (CPid, DetailedSyscallExit)
      m
      ()
filterSuccess = CL.mapMaybe $ \case
  (pid, Right details) -> Just (pid, details)
  (_, Left _) -> Nothing

-- | Convert a stream of Hatrace evens into a stream of cights, i.e. events
-- that we care about.
cights ::
     (MonadIO m, MonadState FdState m)
  => ConduitT (CPid, TraceEvent) Cight m ()
cights = HT.syscallExitDetailsOnlyConduit
      .| filterSuccess
      .| updateFdState
      .| CL.mapMaybeM asCight
