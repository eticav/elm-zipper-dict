module MoveTests exposing (..)

import Expect
import Test exposing (..)

import RedBlackZipper as RBZ
import TestUtils exposing (..)
import Samples exposing (..)

goL : Maybe (RBZ.Zipper Int String)->Maybe (RBZ.Zipper Int String)
goL mZip =
  mZip
  |> Maybe.andThen RBZ.goLeft

goR : Maybe (RBZ.Zipper Int String)->Maybe (RBZ.Zipper Int String)
goR mZip =
  mZip
  |> Maybe.andThen RBZ.goRight
     
goU : Maybe (RBZ.Zipper Int String)->Maybe (RBZ.Zipper Int String)
goU mZip =
  mZip
  |> Maybe.andThen RBZ.goUp

goT : Maybe (RBZ.Zipper Int String)->Maybe (RBZ.Zipper Int String)
goT mZip =
  mZip
  |> Maybe.andThen (Just << RBZ.goTop)
                      
treeZ : List Int -> RBZ.Zipper Int String
treeZ list =
  RBZ.new|>insertZs list

tests : Test
tests =
    describe "zipper movements"
      [ zipperBasicMovements
      , zipperGoToMovements
      ]
    
zipperBasicMovements : Test
zipperBasicMovements =
  let
    zip = Just smallZip          
  in 
    describe "basic movements"
      [ test "left" (valueCheck (zip|>goL) (Just 2))
      , test "right" (valueCheck (zip|>goR) (Just 14))
      , test "up" (valueCheck (zip|>goU) Nothing)
      , test "left-up" (valueCheck (zip|>goL|>goU) (Just 11))
      , test "left-right" (valueCheck (zip|>goL|>goR) (Just 7))
      , test "left-right-left" (valueCheck (zip|>goL|>goR|>goL) (Just 5))
      , test "left-right-left-right" (valueCheck (zip|>goL|>goR|>goL|>goR) Nothing)      
      , test "left-right-left-right-up" (valueCheck (zip|>goL|>goR|>goL|>goR|>goU) (Just 5))
      , test "left-right-left-right-up-up" (valueCheck (zip|>goL|>goR|>goL|>goR|>goU|>goU) (Just 7))
      , test "left-right-left-right-top" (valueCheck (zip|>goL|>goR|>goL|>goR|>goT) (Just 11))      
      ]

zipperGoToMovements : Test
zipperGoToMovements =
  let
    zip = smallZip       
  in 
    describe "GoTo Movements"
      [ test "gotoDown 11" (nodeCheck (zip|>RBZ.goToDown 11) (Just (11,RBZ.Black)))
      , test "gotoDown 1" (nodeCheck (zip|>RBZ.goToDown 1) (Just (1,RBZ.Black)))
      , test "gotoDown 5" (nodeCheck (zip|>RBZ.goToDown 5) (Just (5,RBZ.Red)))
      , test "gotoDown 8" (nodeCheck (zip|>RBZ.goToDown 8) (Just (8,RBZ.Red)))
      , test "gotoDown 14" (nodeCheck (zip|>RBZ.goToDown 14) (Just (14,RBZ.Black)))
      , test "gotoDown No found" (nodeCheck (zip|>RBZ.goToDown 13) Nothing)
      ]

    
valueCheck : Maybe (RBZ.Zipper Int String)->Maybe Int->()->Expect.Expectation
valueCheck zip expected () =  
  case zip of
    Just (RBZ.Node _ key _ _ _,_)->
      Expect.equal expected (Just key)
    Just (RBZ.Empty _,_)->
      Expect.equal expected Nothing
    Nothing ->
      Expect.equal expected Nothing 


