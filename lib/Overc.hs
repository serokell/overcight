{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Overc
  ( overc

  -- reexports
  , Cight (..)
  , cights

  , Cights
  ) where

import Data.Conduit ((.|))
import System.Exit (ExitCode)

import qualified Data.Conduit.List as CL
import qualified System.Hatrace as HT

import Overc.Cight (Cight (CightExec, CightRead))
import Overc.Internal (cights)
import Overc.Internal.Cights (Cights)
import Overc.Internal.FdState (empty)
import RioState (evalState)

import qualified Overc.Internal.Cights as Cights


-- | Trace a command a return cights.
overc :: String -> [String] -> IO (ExitCode, Cights)
-- TODO: Make it take @CreateProcess@.
overc name args = do
    argv <- HT.procToArgv name args
    evalState (HT.sourceTraceForkExecvFullPathWithSink argv conduit) empty
  where
    conduit = cights .| CL.fold (flip Cights.insert) Cights.empty
