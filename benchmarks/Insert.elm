module Insert exposing (..)

import Benchmark as BMK
import Benchmark.Runner as BMKRunner
import BenchmarckUtils exposing (..)
import Dict

import ZDict

import Samples exposing (..)
        
insertRange : BenchmarkInputs->BMK.Benchmark
insertRange  inputs =
  BMK.compare inputs.description
       
       (insertRangeZDict inputs)
       (insertRangeDict inputs)       

insertRangeZDict : BenchmarkInputs->BMK.Benchmark
insertRangeZDict  inputs =
  BMK.benchmark2 "ZDict" zInsertList inputs.list inputs.zDict

insertRangeDict : BenchmarkInputs->BMK.Benchmark
insertRangeDict  inputs =
  BMK.benchmark2 "Dict" insertList inputs.list inputs.dict
       
rangeStr l h = "[" ++ (toString l) ++ "," ++ (toString h) ++ "]"
  
description : Int->Int->List Int->String
description l h list=  
  "insert -- insert "++(toString list) ++" in the range " ++ rangeStr l h

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
    i = inputs 1 1000000 2 [0,3,5,11]
  in 
    BMK.describe "Insert"
         [ --insertRange (inputs 1 100000 2 [0])
             insertRangeZDict i
          --   insertRangeDict i
             --insertRange i
         ]
