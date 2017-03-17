module FoldingTests exposing (..)

import Expect
import Test exposing (..)

import RedBlackZipper as RBZ
import TestUtils exposing (..)

tests : Test
tests =
  describe "Zipper Folding"
  [ zipperFolding
  , zipperKeys
  ]

zipperFolding : Test
zipperFolding =
  let
    zip = RBZ.zipper (n RBZ.Black 11
                      (n RBZ.Red 2
                         (t RBZ.Black 1)
                         (n RBZ.Black 7
                            (t RBZ.Red 5)
                            (t RBZ.Red 8)
                         )
                      )
                      (n RBZ.Black 14
                         l
                         (t RBZ.Red 15)
                      )
                   )          
    fun k v acc = k::acc
  in 
    describe "Zipper Folding"
      [ test "foldl -- get all keys" (foldlCheck fun [] zip [15,14,11,8,7,5,2,1])
      , test "foldr -- get all keys" (foldrCheck fun [] zip [1,2,5,7,8,11,14,15])
      , test "foldr -- get all keys" (foldrCheck fun [] zip [1,2,5,7,8,11,14,15])
      , test "insert range 1..10"
          (foldrCheck fun [] (RBZ.new |> insertZs (List.range 1 5000)) (List.range 1 5000))
      ]
    
foldlCheck : (Int -> String -> List a -> List a)->List a->RBZ.Zipper Int String->List a->()->Expect.Expectation
foldlCheck fun initial zip expected () =
    Expect.equal (RBZ.foldl fun initial zip) expected

foldrCheck : (Int -> String -> List a -> List a)->List a->RBZ.Zipper Int String->List a->()->Expect.Expectation
foldrCheck fun initial zip expected () =
    Expect.equal (RBZ.foldr fun initial zip) expected

        
keysCheck : RBZ.Zipper Int String->List Int->()->Expect.Expectation
keysCheck zip list =
  \_->Expect.equalLists (RBZ.keys zip) list

zipperKeys : Test
zipperKeys =
  let
    zip = RBZ.zipper (n RBZ.Black 11
                      (n RBZ.Red 2
                         (t RBZ.Black 1)
                         (n RBZ.Black 7
                            (t RBZ.Red 5)
                            (t RBZ.Red 8)
                         )
                      )
                      (n RBZ.Black 14
                         l
                         (t RBZ.Red 15)
                      )
                   )
  in 
    describe "RBZ.Zipper keys"
      [ test "insert zip at root" (keysCheck zip [1,2,5,7,8,11,14,15])
      , test "insert 11 1  3 4"
          (keysCheck (RBZ.new |> insertZs [11,1,3,4]) [1,3,4,11])
      , test "insert range 1..20"
          (keysCheck (RBZ.new |> insertZs (List.range 1 20)) (List.range 1 20))
      , test "insert range 1..20"
          (keysCheck (RBZ.new |> insertZs ( List.reverse (List.range 1 20))) (List.range 1 20))          
      , test "insert range 1..100000"
          (keysCheck (RBZ.new |> insertZs ( List.reverse (List.range 1 10000))) (List.range 1 10000))
      , test "insert range many 1"
          (keysCheck (RBZ.new |> insertZs [1,1,1,1,1,1,1,1]) [1])
      , test "insert range many identicals"
          (keysCheck (RBZ.new |> insertZs [1,2,3,1,1,2,3,1,2,3,1,2,3,1]) [1,2,3])
      ]

