module AdventOfCode.Tests

open Fuchu
open Library
open FSharpx.Collections.TimeMeasurement
open System.IO

let testCasePrint name test =
  let runner () =
    time test |> Arrow.second (printfn "%s %A\n" name) |> fst
  testCase name runner

let read file = readFile <| Path.Combine(__SOURCE_DIRECTORY__, "../src/AoC2016/puzzles", file)

module Day01 =
  open AoC.Day01
  let puzzleInput = "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4"
  let results = [
    testCasePrint "Day 01: Part 1" <| fun _ -> Assert.Equal("Distance", 181, distance puzzleInput)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("First intersection distance", 140, firstIntersectionDistance puzzleInput)
  ]

module Day06 =
  open AoC.Day06
  let puzzleInput = read "input-2016-day06.txt"
  let results = [
    testCasePrint "Day 06: Part 1" <| fun _ -> Assert.Equal("Message 1", "kjxfwkdh", findMessageWithMostCommonLetter puzzleInput)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("Message 2", "xrwcsnps", findMessageWithLeastCommonLetter puzzleInput)
  ]

module Day09 =
  open AoC.Day09
  open FParsec.CharParsers
  let puzzleInput = read "input-2016-day09.txt"
  let results = [
    testCasePrint "Day 09: Part 1" <| fun _ -> Assert.Equal("Length 1", 97714L, run grammar puzzleInput |> result)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("Length 2", 10762972461L, run grammarLen >> result <| puzzleInput)
  ]

module Day11 =
  open AoC.Day11
  let puzzleInput1 = read "input-2016-day11-1.txt"
  let puzzleInput2 = read "input-2016-day11-2.txt"
  let results = [
    testCasePrint "Day 11: Part 1" <| fun _ -> Assert.Equal("Steps 1", Some 47, computeSteps puzzleInput1)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("Steps 2", Some 71, computeSteps puzzleInput2)
  ]

module Day15 =
  open AoC.Day15
  let puzzleInput  = [(17, 5);(19, 8);( 7, 1);(13, 7);( 5, 1);( 3, 0)]
  let puzzleInput2 = puzzleInput @ [(11, 0)]
  let results = [
    testCasePrint "Day 15: Part 1" <| fun _ -> Assert.Equal("First slot 1", 16824, findFirstSlot puzzleInput)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("First slot 2", 3543984, findFirstSlot puzzleInput2)
  ]

module Day18 =
  open AoC.Day18
  let puzzleInput = ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
  let results = [
    testCasePrint "Day 18: Part 1" <| fun _ -> Assert.Equal("Safe plates 40 rows", 2013, safeCount 40 puzzleInput)
    // Very slow
//    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("Safe plates 400000 rows", 20006289, safeCount 400000 puzzleInput)
  ]

[<Tests>]
let ``Advent of Code`` () =
  testList "2016" [
    testList "Day 01" Day01.results
    testList "Day 06" Day06.results
    testList "Day 09" Day09.results
    testList "Day 11" Day11.results
    testList "Day 15" Day15.results
    testList "Day 18" Day18.results
  ]
