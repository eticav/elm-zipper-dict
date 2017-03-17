module InsertionTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Dict exposing (..)

import ZDict exposing (..)
import RedBlackZipper as RBZ
import Extra

import TreeConstraintsUtils exposing (..)
import TestUtils exposing (..)

tests : Test
tests =
  describe "Zipper Insertions"
    [ insertTest "insert 11" [11]
    , insertTest "insert [11,1]" [11,1]
    , insertTest "insert [11,12]" [11,12]
    , insertTest "insert [11,12,13]" [11,12,13]
    , insertTest "insert [1..40]" (List.range 1 40)
    , fuzz (Fuzz.list Fuzz.int) "fuzz checking consecutives reds after insertion"
        ( \list-> redTwiceCheck ((RBZ.new |> flip insertZs) list ) )
    , fuzz (Fuzz.list Fuzz.int) "fuzz checking keys after insertion"
        ( \list-> blackCountsCheck ((RBZ.new |> flip insertZs) list ) )          
    ]
      
insertTest : String->List Int->Test
insertTest description list =
  let
    newZip = insertZs list RBZ.new
             
    resultKeys = newZip
               |> RBZ.keys                      
               
    expectedKeys = list |> List.sort
  in      
    describe description
    [ test "checking resulting keys in tree after insertion" (\()->Expect.equalLists resultKeys expectedKeys)
    , treeCheck (newZip)
    ]
