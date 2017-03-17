module Delete exposing (..)

import Benchmark as BMK
import Benchmark.Runner as BMKRunner
import Dict
import BenchmarckUtils exposing (..)
import ZDict

import Samples exposing (..)

        
deleteRange : BenchmarkInputs->BMK.Benchmark
deleteRange  inputs =
  BMK.compare inputs.description       
  (deleteRangeZDict inputs)
  (deleteRangeDict inputs)       

deleteRangeZDict : BenchmarkInputs->BMK.Benchmark
deleteRangeZDict  inputs =
  BMK.benchmark2 "ZDict" zDeleteList inputs.list inputs.zDict

deleteRangeDict : BenchmarkInputs->BMK.Benchmark
deleteRangeDict  inputs =
  BMK.benchmark2 "Dict" deleteList inputs.list inputs.dict
       
rangeStr l h = "[" ++ (toString l) ++ "," ++ (toString h) ++ "]"
  
description : Int->Int->List Int->String
description l h list=  
  "delete -- delete "++(toString list) ++" in the range " ++ rangeStr l h

type alias BenchmarkInputs = { list  : List Int     
                             , dict : Dict.Dict Int String
                             , zDict : ZDict.ZDict Int String
                             , description : String
                             }

inputs l h step list =
  let
    initialValues = (listRangeStep l h step)                         
  in 
    { list = list
    , dict = insertList initialValues Dict.empty
    , zDict = zInsertList initialValues ZDict.empty
    , description = description l h list
    }
    
suite : BMK.Benchmark
suite =
  let
    i = inputs 1 100000 2 [10,100,1000,10000,100000]
  in 
    BMK.describe "Delete"
         [ --deleteRange (inputs 1 100000 2 [0])
             deleteRangeZDict i
         --, deleteRangeDict i
             --deleteRange i
         ]


