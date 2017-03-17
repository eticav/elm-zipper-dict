module Main exposing (..)

import Test exposing (..)
import Test.Runner.Html

import TreeConstraintsUtils as ConstraintsUtils
import InsertionTests as Insert
import DeletionTests as Delete
import FoldingTests as Fold
import MoveTests as Move

main : Test.Runner.Html.TestProgram
main =
  [ Move.tests
  , Insert.tests
  , Delete.tests
  , Fold.tests
  , ConstraintsUtils.tests
  ]
     |> concat
     |> Test.Runner.Html.run
        
