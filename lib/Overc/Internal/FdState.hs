{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Processing state.
--
-- Detatils for syscalls like read carry only the fd number, so we have
-- to manually track open and close calls and maintain a map from
-- open file descriptors to file paths.
module Overc.Internal.FdState
  ( FdState
  , empty
  , updateFdState
  , Map.lookup
  ) where

import Control.Monad.State (MonadState, gets, modify)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT)
import Data.Map (Map)
import Foreign.C.Types (CInt)
import System.Hatrace (DetailedSyscallExit)
import System.Posix.Types (CPid)

import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified System.Hatrace as HT


-- TODO: include some header

const_AT_FDCWD :: Integral n => n
const_AT_FDCWD = -100


-- TODO: This approach is questionable as files can be moved around and thus
-- their names can change.

-- | A map from open file descriptors to their paths.
type FdState = Map (CPid, CInt) ByteString

-- | A new empty @FdState@.
empty :: FdState
empty = Map.empty

-- TODO: track fd inheritance

-- | A conduit that updates the @FdState@ map on @open@ and @close@ calls.
updateFdState ::
     MonadState FdState m
  => ConduitT (CPid, DetailedSyscallExit) (CPid, DetailedSyscallExit) m ()
updateFdState = CL.iterM $ \(pid, detail) -> case detail of
  -- TODO: Resolve relative file path
  HT.DetailedSyscallExit_open det ->
    let
      fd = HT.fd (det :: HT.SyscallExitDetails_open)
      fp = HT.pathnameBS (HT.enterDetail (det :: HT.SyscallExitDetails_open) :: HT.SyscallEnterDetails_open)
    in modify $ Map.insert (pid, fd) fp
  HT.DetailedSyscallExit_openat det ->
    let
      fd = HT.fd (det :: HT.SyscallExitDetails_openat)
      dirfd = HT.dirfd (HT.enterDetail (det :: HT.SyscallExitDetails_openat) :: HT.SyscallEnterDetails_openat)
      fp = HT.pathnameBS (HT.enterDetail (det :: HT.SyscallExitDetails_openat) :: HT.SyscallEnterDetails_openat)
    in
      if dirfd == const_AT_FDCWD
      -- TODO: Resolve relative file path
      then modify $ Map.insert (pid, fd) fp
      else gets (Map.lookup (pid, dirfd)) >>= \case
        Nothing -> error "updateFdState: failed tracking dirfd for openat"
        Just dirp -> modify $ Map.insert (pid, fd) (dirp <> "/" <> fp)
  HT.DetailedSyscallExit_close det ->
    let
      fd = HT.fd (HT.enterDetail (det :: HT.SyscallExitDetails_close) :: HT.SyscallEnterDetails_close)
    in modify $ Map.delete (pid, fd)
  _ -> pure ()
