{- © 2019 Kirill Elagin <kir@elagin.me>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Overc.Persist
  ( toFileName
  ) where

import System.FilePath ((</>), takeBaseName)


toFileName :: String -> String
toFileName name = "overc-" </> takeBaseName name </> ".json"
