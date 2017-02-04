module AdventOfCode.Tests

open Fuchu

module Day01 =
  open AoC.Day01
  let puzzleInput = "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4"
  let results = [
    testCase "Part 1" <| fun _ -> Assert.Equal("Distance", 181, distance puzzleInput)
    testCase "Part 2" <| fun _ -> Assert.Equal("First intersection distance", 140, firstIntersectionDistance puzzleInput)
  ]

module Day15 =
  open AoC.Day15
  let puzzleInput  = [(17, 5);(19, 8);( 7, 1);(13, 7);( 5, 1);( 3, 0)]
  let puzzleInput2 = puzzleInput @ [(11, 0)]
  let results = [
    testCase "Part 1" <| fun _ -> Assert.Equal("First slot 1", 16824, findFirstSlot puzzleInput)
    testCase "Part 2" <| fun _ -> Assert.Equal("First slot 2", 3543984, findFirstSlot puzzleInput2)
  ]

module Day18 =
  open AoC.Day18
  let puzzleInput = ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
  let results = [
    testCase "Part 1" <| fun _ -> Assert.Equal("Safe plates 40 rows", 2013, safeCount 40 puzzleInput)
    // Very slow
    // testCase "Part 2" <| fun _ -> Assert.Equal("Safe plates 400000 rows", 20006289, safeCount 400000 puzzleInput)
  ]

[<Tests>]
let ``Advent of Code`` () =
  testList "2016" [
    testList "Day 01" Day01.results
    testList "Day 15" Day15.results
    testList "Day 18" Day18.results
  ]
