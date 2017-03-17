module Get exposing (..)

import Benchmark as BMK
import Benchmark.Runner as BMKRunner
import Dict

import ZDict

import Samples exposing (..)

getList : List Int->Dict.Dict Int String->List (Maybe String)
getList list dict =
  let
    getIt x = Dict.get x dict
  in 
    list |> List.map getIt

zGetList : List Int->ZDict.ZDict Int String->List (Maybe String)
zGetList list dict =
  let
    getIt x = ZDict.get x dict
  in 
    list |> List.map getIt


rangeStr l h = "[" ++ (toString l) ++ "," ++ (toString h) ++ "]"
stepStr step = " (step " ++ (toString step) ++")"

description : Int->Int->Int->String
description l h step =  
  "get -- get all keys" ++ stepStr step++" in the range " ++ rangeStr l h ++ " in a dict "
         
getRange : String->List Int->Dict.Dict Int String ->ZDict.ZDict Int String->BMK.Benchmark
getRange  description list dict zDict =
  BMK.compare description
  ( BMK.benchmark2 "ZDict" zGetList list zDict )
  ( BMK.benchmark2 "Dict" getList list dict )
  

  
  
    
suite : BMK.Benchmark
suite =
  let
    dictXXS = dictRange 1 1
    zDictXXS = zDictRange 1 1 
    listXXS = listRangeStep 1 1 3
    descriptionXXS = description 1 1 3

    -- dictXS = dictRange 1 10
    -- zDictXS = zDictRange 1 10 
    -- listXS = listRangeStep 1 10 3
    -- descriptionXS = description 1 10 3

    -- dictM = dictRange 1 100
    -- zDictM = zDictRange 1 100 
    -- listM = listRangeStep 1 100 7
    -- descriptionM = description 1 100 7

    -- dictL = dictRange 1 1000
    -- zDictL = zDictRange 1 1000 
    -- listL = listRangeStep 1 1000 97
    -- descriptionL = description 1 1000 97

    -- dictXL = dictRange 1 10000
    -- zDictXL = zDictRange 1 10000 
    -- listXL = listRangeStep 1 10000 997
    -- descriptionXL = description 1 10000 997

    dictXXL = dictRange 1 100000
    zDictXXL = zDictRange 1 100000 
    listXXL = listRangeStep 1 100000 997
    descriptionXXL = description 1 100000 997
                     
    -- dictXXXL = dictRange 1 1000000
    -- zDictXXXL = zDictRange 1 1000000 
    -- listXXXL = listRangeStep 1 1000000 97
    -- descriptionXXXL = description 1 1000000 97
    
  in 
    BMK.describe "Get"
         [ getRange descriptionXXS listXXS dictXXS zDictXXS
--         getRange descriptionXS listXS dictXS zDictXS
     --     getRange descriptionM listM dictM zDictM
--         getRange descriptionL listL dictL zDictL
--         , getRange descriptionXL listXL dictXL zDictXL
   --      , getRange descriptionXXL listXXL dictXXL zDictXXL

--         , getRange descriptionXXXL listXXXL dictXXXL zDictXXXL
         ]
