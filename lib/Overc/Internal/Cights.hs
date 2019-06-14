{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | A set of cights with their kinds.
--
-- We assume that “exec” is a superset of “read”, that is, everything that
-- was executed is counted as being read to.
module Overc.Internal.Cights
  ( Cights

  , empty
  , insert

  , allotted
  ) where

import Prelude hiding (read)

import Data.Set (Set)
import Fmt (Buildable (..), indentF, blockListF)
import Lens.Micro (over)
import Lens.Micro.TH (makeLenses)

import qualified Data.Set as Set

import Overc.Cight (Cight (..), CightPath)


data Cights = Cights
  { _cightsRead :: Set CightPath
  , _cightsExec :: Set CightPath
  }

$(makeLenses ''Cights)

instance Buildable Cights where
  build (allotted -> (read, exec))
    = "read:\n" <> indentF 2 (blockListF read)
   <> "exec:\n" <> indentF 2 (blockListF exec)


-- | Create a new empty @Cights@ structure.
empty :: Cights
empty = Cights Set.empty Set.empty

-- | Add a new cight to the set of cights.
insert :: Cight -> Cights -> Cights
insert (CightRead fp) = over cightsRead (Set.insert fp)
insert (CightExec fp) = over cightsExec (Set.insert fp)


-- | Turns @Cights@ into a pair of sets (raeds, execs) where the sets do
-- not inersect, i.e. each path is classified as /either/ read or exec
-- (if a path was both read and execd it is counted as exec).
allotted :: Cights -> (Set CightPath, Set CightPath)
allotted Cights{_cightsRead, _cightsExec} =
  (_cightsRead `Set.difference` _cightsExec, _cightsExec)
