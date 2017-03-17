module DeletionTests exposing (..)

import Expect
import Test exposing (..)
import Dict exposing (..)

import ZDict exposing (..)
import RedBlackZipper as RBZ
import Extra

import TreeConstraintsUtils exposing (..)
import TestUtils exposing (..)
import Samples exposing (..)

tests : Test
tests =    
  describe "Zipper Deletions"
    [ removeMaxTest
    , zipperSimpleDeletions
    , zipperComplexDeletions
    ]
    
zipperSimpleDeletions : Test
zipperSimpleDeletions =
  let
     zip = RBZ.zipper (n RBZ.Black 11
                      (n RBZ.Red 2
                         (t RBZ.Black 1)
                         (n RBZ.Black 7
                            (t RBZ.Red 5)
                            l
                         )
                      )
                      (n RBZ.Black 14
                         l
                         (t RBZ.Red 15)
                      )
                   )    
  in      
    describe "Simple Deletions"
    [ deleteTest "delete terminal Red 5 in zip" 5 zip
    , deleteTest "delete not found" 8 zip
    , deleteTest "delete semi-terminal Black with Left Red child 2 in zip " 7 zip
    , deleteTest "delete semi-terminal Black with Right Red child 2 in zip " 14 zip
    , deleteTest "delete terminal black in zip " 1 zip
    ]

zipperComplexDeletions : Test
zipperComplexDeletions = 
  describe "complex Deletions"
    [ deleteTest "delete Red : Black Black" 2 smallZip
    , deleteTest "delete Black : Red Red" 7 smallZip
    , deleteStepInRange "deletion multiple of 7 in 1..1000" 7 1000
    , deleteStepInRange "deletion multiple of 2 in 1..1000" 2 1000
    , deleteStepInRange "deletion multiple of 2 in 1..1000" 1 1000
    ]

deleteStepInRange : String->Int->Int->Test
deleteStepInRange description step range=
  let
    list = List.range 1 range
    removeKeys = list |> List.filter (\x->rem x step==0)
    expectedKeys = list |> List.filter (\x->rem x step/=0)

    zip = insertZs list RBZ.new
    
    newZip = removeZs removeKeys zip
                 
    resultKeys = newZip
               |> RBZ.keys
                  
    allKeys = zip
            |> RBZ.keys               
  in      
    describe description
    [ test "checking resulting keys in tree after deletions " (\()->Expect.equalLists resultKeys expectedKeys)
    , treeCheck (newZip)
    ]
  
deleteTest : String->Int->RBZ.Zipper Int String->Test
deleteTest description key zip =
  let
    newZip =removeZ key zip
             
    resultKeys = newZip
               |> RBZ.keys
                  
    allKeys = zip
            |> RBZ.keys
               
    expectedKeys = allKeys |> List.filter (\x->x/=key)
  in      
    describe description
    [ test "checking resulting keys in tree after deletion" (\()->Expect.equalLists resultKeys expectedKeys)
    , treeCheck (newZip)
    ]
        
removeMaxTest : Test
removeMaxTest =
  let
     zipRed = smallZip
     zipBlack = mediumZip
     zipBlack2 = RBZ.updateMaybe RBZ.goLeft mediumZip
  in      
    describe "Remove max"
    [ maxTest "max is black" zipRed
    , maxTest "max is black" zipBlack
    , maxTest "max is black previous is red" zipBlack2
    ]

maxTest : String->RBZ.Zipper Int String->Test
maxTest description zip =
  let
    ((key,v), newZip)= RBZ.removeMax (Tuple.first zip)
    resultKeys = RBZ.keys (RBZ.zipper newZip)
    allKeys = RBZ.keys zip
    max = Maybe.withDefault 0 (List.maximum allKeys)
    expectedKeys = allKeys |> List.filter (\x->x/=max) |> List.sort
  in      
    describe description
    [ test "checking max key" (\()->Expect.equal key max)
    , test "checking resulting keys in tree after max removal" (\()->Expect.equalLists resultKeys expectedKeys)
    , treeCheck (RBZ.zipper newZip)
    ]
  
