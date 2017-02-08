(*
  AoC - Day 6

  http://adventofcode.com/2016/day/6
*)
module AoC.Day06

open Library

let parser (str: string) = str.Split('\n')

let f acc (line : string) =
  line
  |> Seq.mapi (fun i c -> i, c)
  |> Seq.fold (fun acc' (i, c) ->
    // Find column `Map` by char index
    match Map.tryFind i acc' with
    | Some m ->
      // Find char `Map`
      match Map.tryFind c m with
      | Some rank ->
        Map.add i (Map.add c (rank+1) m) acc'
      | None ->
        Map.add i (Map.add c 0 m) acc'
    | None ->
      Map.add i (Map.add c 0 Map.empty) acc'
  ) acc

let charsToString chs = new System.String(chs |> List.toArray)

let wordRank input =
  parser input
  |> Seq.fold f Map.empty
  |> Map.toList
  |> List.map (snd >> Map.toList)

let findMessageWith letterLeastMost input =
  wordRank input
  |> List.map (letterLeastMost snd >> fst)
  |> charsToString

// PART 1
let findMessageWithMostCommonLetter = findMessageWith List.maxBy

// PART 2
let findMessageWithLeastCommonLetter = findMessageWith List.minBy
