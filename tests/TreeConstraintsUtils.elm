module TreeConstraintsUtils exposing (..)

import Expect
import Test exposing (..)
import RedBlackZipper as RBZ
import TestUtils exposing (..)

type ColoredNode comparable v = CNode RBZ.Color comparable v                              

accDown : String -> 
          RBZ.Color->
          comparable->
          v->
          (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))->
          (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))
accDown str color k v (cur,acc) =
  let    
    res = ((CNode color k v)::cur,acc)
  in
    res 

accUp : String->
        (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))->
        (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))
accUp str (cur,acc) =
  let
    newCur = case cur of
               []->[]
               head::tail->tail
    res=(newCur,acc)
  in
    res

accLeaf : String->
          (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))->
          (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))
accLeaf str (cur,acc) =
  let
    res=(cur,cur::acc)
  in
    res 
    
paths : RBZ.Zipper comparable v->
        (List (ColoredNode comparable v),List (List (ColoredNode comparable v)))
paths zip =
  let 
    (res,_)=RBZ.each
             (accDown " - down -")
             (accLeaf "- leaf -")
             (accUp "- up -")             
             (([],[]), zip)
  in
    res

blackCountsCheck : RBZ.Zipper Int String->Expect.Expectation
blackCountsCheck zip =
  (Tuple.second (paths zip)
    |> blackCountsCheckImpl ) ()
      
blackCountsCheckImpl : List (List (ColoredNode comparable v))->
                   ()->Expect.Expectation
blackCountsCheckImpl allPaths =
  let                   
    addBlack cNode acc = case cNode of
                           CNode RBZ.Black _ _->acc+1
                           _->acc
                              
    countBlacks list = List.foldr addBlack 0 list
    allBlacksCounted = List.map (\x->countBlacks x) allPaths
  in
    case List.maximum allBlacksCounted == List.minimum allBlacksCounted of
      True -> \_->Expect.pass
      False -> \_->Expect.fail (toString allPaths)

redTwiceCheck : RBZ.Zipper Int String->Expect.Expectation
redTwiceCheck zip =
  (Tuple.second (paths zip)
    |> redTwiceCheckImpl) ()
      
redTwiceCheckImpl : List (List (ColoredNode comparable v))->
                   ()->Expect.Expectation
redTwiceCheckImpl allPaths =
  let
                   
    checkColor cNode (color,result) = case cNode of
                                        CNode RBZ.Red _ _->(RBZ.Red, result || (color==RBZ.Red))
                                        _->(RBZ.Black,result)
                              
    checkColorsInList list = List.foldr checkColor (RBZ.Black,False) list
    allColorsChecked = List.map (\x->checkColorsInList x) allPaths
  in
    case List.foldl (\(_,c) acc-> acc || c ) False allColorsChecked of
      True -> \_->Expect.fail (toString allPaths)
      False -> \_->Expect.pass
      

treeCheck : RBZ.Zipper Int String->Test
treeCheck zip =
  let
    (_,allPaths) = paths zip    
  in 
    describe "Tree consistency"
      [ test "black counts " (blackCountsCheckImpl allPaths)
      , test "red Twice Check " (redTwiceCheckImpl allPaths)
      ]
    
tests : Test
tests =
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

    zip2 = RBZ.zipper (n RBZ.Black 2
                        (t RBZ.Red 1)
                        (t RBZ.Red 11)
                      )
           
    zipFail = RBZ.zipper (n RBZ.Black 2
                         (t RBZ.Red 1)
                         (t RBZ.Red 11)
                         )
  in 
    describe "checking if the test functions that check tree consitency work"
      [ treeCheck zip
      , treeCheck zipFail
      ]



