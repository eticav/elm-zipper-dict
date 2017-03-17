module Samples exposing (..)

import Dict
import ZDict

dictInsert : Int->Dict.Dict Int String->Dict.Dict Int String
dictInsert i dict=
  Dict.insert i (toString i) dict

zDictInsert : Int->ZDict.ZDict Int String ->ZDict.ZDict Int String
zDictInsert i zdict=
  ZDict.insert i (toString i) zdict



dictDelete : Int->Dict.Dict Int String->Dict.Dict Int String
dictDelete i dict=
  Dict.remove i dict

zDictDelete : Int->ZDict.ZDict Int String ->ZDict.ZDict Int String
zDictDelete i zdict=
  ZDict.remove i zdict

  
  
dictRange : Int->Int->Dict.Dict Int String
dictRange l h =
  let
    list = List.range l h    
  in
    List.foldl dictInsert Dict.empty list

zDictRange : Int->Int->ZDict.ZDict Int String
zDictRange l h =
  let
    list = List.range l h    
  in
    List.foldl zDictInsert ZDict.empty list

listRangeStep : Int->Int->Int->List Int
listRangeStep l h step =
  let 
    list = List.range l h
    filter x =
      case x - 1 of
        0 -> Just x
        v -> case rem x step of
               0-> Just x
               _-> Nothing
  in
    List.filterMap filter list
            
dictXXXS = dictRange 1 1
zdictXXXS = zDictRange 1 1
    
listXXS = List.range 0 10
dictXXS = dictRange 0 10
zdictXXS = zDictRange 0 10

              
