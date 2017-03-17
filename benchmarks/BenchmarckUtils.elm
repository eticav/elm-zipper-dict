module BenchmarckUtils exposing (..)

import Dict
import ZDict

import Samples exposing (..)

insertList : List Int->Dict.Dict Int String->Dict.Dict Int String
insertList list dict =
  List.foldl dictInsert dict list

zInsertList : List Int->ZDict.ZDict Int String->ZDict.ZDict Int String
zInsertList list zdict=
  List.foldl zDictInsert zdict list

deleteList : List Int->Dict.Dict Int String->Dict.Dict Int String
deleteList list dict =
  List.foldl dictDelete dict list

zDeleteList : List Int->ZDict.ZDict Int String->ZDict.ZDict Int String
zDeleteList list zdict=
  List.foldl zDictDelete zdict list
