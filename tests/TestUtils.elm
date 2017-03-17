module TestUtils exposing (..)

import Expect
import Test exposing (..)

import ZDict exposing (..)
import RedBlackZipper as RBZ

l : RBZ.Tree Int String
l = RBZ.empty
    
n : RBZ.Color-> Int -> RBZ.Tree Int String->RBZ.Tree Int String->RBZ.Tree Int String
n color k left right =
  RBZ.Node color k (toString k) left right

t : RBZ.Color-> Int -> RBZ.Tree Int String
t color k =
  RBZ.Node color k (toString k) l l


insertZ : Int->RBZ.Zipper Int String -> RBZ.Zipper Int String
insertZ k zip =
  RBZ.insert k (toString k) zip |> RBZ.goTop

insertZs : List Int->RBZ.Zipper Int String -> RBZ.Zipper Int String
insertZs list zip =
  List.foldl insertZ zip list


removeZ : Int->RBZ.Zipper Int String -> RBZ.Zipper Int String
removeZ k zip =
  RBZ.remove k zip |> RBZ.goTop

removeZs : List Int->RBZ.Zipper Int String -> RBZ.Zipper Int String
removeZs list zip =
  List.foldl removeZ zip list
  
nodeCheck : RBZ.Zipper Int String-> Maybe (Int,RBZ.Color)->()->Expect.Expectation
nodeCheck zip expected () =  
  case zip of
    (RBZ.Node color key _ _ _,_)->
      Expect.equal expected (Just (key,color))
    (RBZ.Empty color,_)->
      Expect.equal expected Nothing
