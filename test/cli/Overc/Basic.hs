module Overc.Basic
  ( unit_simple
  , unit_exec
  ) where

import Data.List (sort)

import Test.Tasty.HUnit ((@?=))

import Common (ShellTest (..), shells, shellTest, writeShellScript)


unit_simple :: IO ()
unit_simple = do
  out <- shellTest $ ShellTest
    { setup = shells
        [ "mkdir a"
        , "mkdir a/b"
        , "touch a/a1"
        , "touch a/a2"
        , "touch a/b/b1"
        , "touch a/b/b2"
        , "overc run -- $TEST_ROOT/Overc/basic/simple.sh"
        ]
    , teardown = shells
        [ "rm -rf a"
        ]
    , run = "overc set view overc-simple.sh.json"
    }
  sort out @?=
    [ "read:"
    , "  a"
    , "  a/b"
    , "  a/a1"
    , "  a/a2"
    ]

unit_exec :: IO ()
unit_exec = do
  out <- shellTest $ ShellTest
    { setup = do
        shells
          [ "mkdir a"
          , "echo hello > a/readme"
          , "touch a/execme"
          ]
        writeShellScript "a/execme" "cat a/readme"
        shells
          [ "overc run -- $TEST_ROOT/Overc/basic/exec.sh"
          ]
    , teardown = shells
        [ "rm -rf a"
        ]
    , run = "overc set view overc-exec.sh.json"
    }
  sort out @?=
    [ "read:"
    , "  a/readme"
    , "exec:"
    , "  a/execme"
    ]
