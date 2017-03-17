module Extra exposing (..)
import ZDict exposing (..)
import RedBlackZipper as RBZ

-- polyLeftFoldl : (RBZ.Tree comparable v->acc->acc)->
--                 (RBZ.Tree comparable v->acc->acc)->
--                 (RBZ.Tree comparable v->acc->acc)->
--                 (RBZ.Tree comparable v->acc->acc)->
--                 (acc, RBZ.Zipper comparable v)->
--                 (acc, RBZ.Zipper comparable v)
-- polyLeftFoldl fLeft fRight fUpFromLeft fUpFromRight (acc,zip) =
--   let
--     newAcc = fLeft (Tuple.first zip) acc
--   in
--     case RBZ.goLeft zip of
--       Just x ->
--         polyLeftFoldl fLeft fRight fUpFromLeft fUpFromRight (newAcc,x)
--       Nothing->
--         polyRightFoldl fLeft fRight fUpFromLeft fUpFromRight (newAcc,zip)


polyLeftFoldl : (RBZ.Tree comparable v->acc->acc)->
                (RBZ.Tree comparable v->acc->acc)->
                (RBZ.Tree comparable v->acc->acc)->
                (RBZ.Tree comparable v->acc->acc)->
                (acc, RBZ.Zipper comparable v)->
                (acc, RBZ.Zipper comparable v)
polyLeftFoldl fLeft fRight fUpFromLeft fUpFromRight (acc,zip) =
  let
    newAcc = fLeft (Tuple.first zip) acc
  in
    case zip of
      (RBZ.Node color k v left right, breadcrumbs)->
        polyLeftFoldl fLeft fRight fUpFromLeft fUpFromRight ( newAcc
                                                            , (left, (RBZ.RightContext color k v right)::breadcrumbs))
      _->polyRightFoldl fLeft fRight fUpFromLeft fUpFromRight (acc,zip)

                       
      
polyRightFoldl : (RBZ.Tree comparable v->acc->acc)->
                 (RBZ.Tree comparable v->acc->acc)->
                 (RBZ.Tree comparable v->acc->acc)->
                 (RBZ.Tree comparable v->acc->acc)->
                 (acc, RBZ.Zipper comparable v)->
                 (acc, RBZ.Zipper comparable v)
polyRightFoldl fLeft fRight fUpFromLeft fUpFromRight (acc,zip) =
  let
    newAcc = fRight (Tuple.first zip) acc
  in
    case RBZ.goRight zip of
      Just x->
        polyLeftFoldl fLeft fRight fUpFromLeft fUpFromRight (newAcc,x)
      Nothing->
        polyUpFoldl fLeft fRight fUpFromLeft fUpFromRight (newAcc,zip)

tStr (tree,_) =
  case tree of
    RBZ.Node color comparable _ _ _-> "Node : " ++ (toString comparable)
    RBZ.Empty color->"Empty"

polyUpFoldl : (RBZ.Tree comparable v->acc->acc)->
              (RBZ.Tree comparable v->acc->acc)->
              (RBZ.Tree comparable v->acc->acc)->
              (RBZ.Tree comparable v->acc->acc)->
              (acc, RBZ.Zipper comparable v)->
              (acc, RBZ.Zipper comparable v)
polyUpFoldl fLeft fRight fUpFromLeft fUpFromRight (acc,zip) =
  case zip of
    (tree, [])->(acc,zip)
                
    (tree, (RBZ.RightContext color k v right)::tail)->
      let
        newAcc = fUpFromLeft tree acc
      in
        polyRightFoldl fLeft fRight fUpFromLeft fUpFromRight (newAcc,(RBZ.Node color k v tree right,tail))
                       
    (tree, (RBZ.LeftContext color k v left)::tail)->
      let
        newAcc = fUpFromRight tree acc
      in
        polyUpFoldl fLeft fRight fUpFromLeft fUpFromRight (newAcc,(RBZ.Node color k v left tree,tail))
        
polyFoldl : (RBZ.Tree comparable v->acc->acc)->
            (RBZ.Tree comparable v->acc->acc)->
            (RBZ.Tree comparable v->acc->acc)->
            (RBZ.Tree comparable v->acc->acc)->
            acc->
            RBZ.Zipper comparable v->
            acc            
polyFoldl fLeft fRight fUpFromLeft fUpFromRight acc zip =
  Tuple.first (polyLeftFoldl fLeft fRight fUpFromLeft fUpFromRight (acc,zip))

map : (RBZ.Tree comparable v->a)->RBZ.Zipper comparable v->List a
map f zip =
  let
    doNothing t acc = acc
    fUpFromRight t acc =      
        (f t)::acc
  in
    List.reverse (polyFoldl doNothing fUpFromRight doNothing doNothing [] zip)

maybeMap : (RBZ.Tree comparable v->Maybe a)->RBZ.Zipper comparable v->List a
maybeMap f zip =
  let
    doNothing t acc = acc
    fUpFromRight t acc =
      case f t of
        Just x->x::acc
        Nothing->acc
  in
    List.reverse (polyFoldl doNothing fUpFromRight doNothing doNothing [] zip)
    
keys : RBZ.Zipper comparable v->List comparable
keys zip =
  let
    key tree = case tree of
                 RBZ.Node _ k _ _ _-> Just k
                 RBZ.Empty _->Nothing
  in
    maybeMap key zip

