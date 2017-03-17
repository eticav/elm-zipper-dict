module Main exposing (..)

import Benchmark as BMK
import Benchmark.Runner as BMKRunner

import Get exposing (suite)
import Insert exposing (suite)
import Delete exposing (suite)

suite : BMK.Benchmark
suite =
  BMK.describe "ZDict" [ --Get.suite
                           --Insert.suite
                           Delete.suite
                       ]

main : BMKRunner.BenchmarkProgram
main =
    BMKRunner.program suite
