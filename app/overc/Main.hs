{- © 2019 Serokell
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Fmt (pretty)
import System.Environment (getArgs)

import qualified Data.Text.IO as T

import Overc (overc)
import Overc.Persist (toFileName)


main :: IO ()
main = do
  cmd : rest <- getArgs
  case cmd of
    "run" -> do
      let "--" : name : args = rest
      let fname = toFileName name
      (_code, cights) <- overc name args
      T.writeFile fname $ pretty cights
    "set" -> do
      let "view" : fname : [] = rest
      T.readFile fname >>= T.putStr
