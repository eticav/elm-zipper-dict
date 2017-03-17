module ZDict exposing (..)

import RedBlackZipper as RBZ

type ZDict comparable v = ZDict (RBZ.Zipper comparable v)
                              
new : ZDict comparable v
new = ZDict RBZ.new

empty : ZDict comparable v
empty = ZDict RBZ.new

insert : comparable -> v->ZDict comparable v -> ZDict comparable v
insert key value zDict =
  case zDict of
    ZDict zip->
     ZDict (RBZ.insert key value zip |> RBZ.goTop)

remove : comparable ->ZDict comparable v->ZDict comparable v
remove key zDict =
  case zDict of
    ZDict zip->
     ZDict (RBZ.remove key zip |> RBZ.goTop)
     

get : comparable->ZDict comparable v->Maybe v
get k zDict =
  case zDict of
    ZDict zip->
      RBZ.get k zip
