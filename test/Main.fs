module AdventOfCode.Tests

open Util
open System.IO
open Xunit

let testCasePrint name test =
  let runner () =
    time test |> Arrow.second (printfn "%s %s" name) |> fst
  runner ()

let read file = readFile <| Path.Combine(__SOURCE_DIRECTORY__, "../src/AoC2016/puzzles", file)

module AoC2016 =
  open AoC.Day01
  let [<Fact>] ``Day 01`` () =
    let puzzleInput = "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4"
    testCasePrint "Day 01: Part 1" <| fun _ -> Assert.Equal(181, distance puzzleInput)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal(140, firstIntersectionDistance puzzleInput)

  open AoC.Day06
  let [<Fact>] ``Day 06`` () =
    let puzzleInput = read "input-2016-day06.txt"
    testCasePrint "Day 06: Part 1" <| fun _ -> Assert.Equal("kjxfwkdh", findMessageWithMostCommonLetter puzzleInput)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal("xrwcsnps", findMessageWithLeastCommonLetter puzzleInput)

  open AoC.Day09
  open FParsec.CharParsers
  let [<Fact>] ``Day 09`` () =
    let puzzleInput = read "input-2016-day09.txt"
    testCasePrint "Day 09: Part 1" <| fun _ -> Assert.Equal(97714L, run grammar puzzleInput |> result)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal(10762972461L, run grammarLen >> result <| puzzleInput)

  open AoC.Day11
  let [<Fact>] ``Day 11`` () =
    let puzzleInput1 = read "input-2016-day11-1.txt"
    let puzzleInput2 = read "input-2016-day11-2.txt"
    testCasePrint "Day 11: Part 1" <| fun _ -> Assert.Equal(Some 47, computeSteps puzzleInput1)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal(Some 71, computeSteps puzzleInput2)

  open AoC.Day15
  let [<Fact>] ``Day 15`` () =
    let puzzleInput  = [(17, 5);(19, 8);( 7, 1);(13, 7);( 5, 1);( 3, 0)]
    let puzzleInput2 = puzzleInput @ [(11, 0)]
    testCasePrint "Day 15: Part 1" <| fun _ -> Assert.Equal(16824, findFirstSlot puzzleInput)
    testCasePrint "        Part 2" <| fun _ -> Assert.Equal(3543984, findFirstSlot puzzleInput2)

  open AoC.Day18
  let [<Fact>] ``Day 18`` () =
    let puzzleInput = ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
    testCasePrint "Day 18: Part 1" <| fun _ -> Assert.Equal(2013, safeCount 40 puzzleInput)
    // Very slow
    // testCasePrint "        Part 2" <| fun _ -> Assert.Equal(20006289, safeCount 400000 puzzleInput)
