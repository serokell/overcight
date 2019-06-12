{- © 2019 Serokell
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Overc.Ls
  ( unit_runLs
  ) where

import Data.List (sort)

import Test.Tasty.HUnit ((@?=))

import Common (ShellTest (..), shells, shellTest)


unit_runLs :: IO ()
unit_runLs = do
  out <- shellTest $ ShellTest
    { setup = shells
        [ "mkdir a"
        , "mkdir a/b"
        , "touch a/a1"
        , "touch a/a2"
        , "touch a/b/b1"
        , "overc run -- ls a"
        ]
    , teardown = shells
        [ "rm -rf a"
        ]
    , run = "overc set view overc-ls.json"
    }
  sort out @?=
    [ "read:"
    , "  a"
    ]
