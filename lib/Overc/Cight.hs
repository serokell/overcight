{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveAnyClass #-}

-- | Defines the primary event type we are watching for.
module Overc.Cight
  ( Cight (CightRead, CightExec)

  , CightPath
  , mkCightPath
  ) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T


type CightPath = FilePath

mkCightPath :: ByteString -> CightPath
mkCightPath = T.unpack . T.decodeUtf8With T.lenientDecode

-- | The primary event we are observing.
data Cight
  = CightRead CightPath  -- ^ A read (or something implying a read) of the path
  | CightExec CightPath  -- ^ An exec (or something implying an exec) of the path
  deriving (Eq, Generic, Hashable, Show)
