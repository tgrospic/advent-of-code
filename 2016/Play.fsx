#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../packages/FSharpx.Collections/lib/net40/FSharpx.Collections.dll"
#load "Library.fs"

open FParsec
open FParsec.Primitives
open Library
open FParsecOp
open FSharpx.Collections

#load "Day11.fs"
open AoC.Day11

type Thing = RTG of string | MEM of string

let check xs =
  let f acc x =
    let chs, gens = acc
    match x with
    | MEM s ->
      if List.contains (RTG s) xs
      then acc
      else (x::chs, gens)
    | RTG s  ->
      if List.contains (MEM s) xs
      then acc
      else (chs, x::gens)
  match List.fold f ([],[]) xs with
  | (x::_), (y::_) -> false
  | _              -> true

type Path = Node of int * (Thing list list) * Thing list * (Thing list list)

// Calculate path cost to the end
let pathCost (Node (n, dns, c, ups)) =
  let xss =
    List.append (List.rev <| dns) (c::ups)
    |> List.rev
  List.mapi (fun i ts -> List.length ts * i) xss
  |> List.sum

let mkPath s =
  let rest = List.rev s
  Node (0, [], List.head rest, List.tail rest)

let rec combinations acc input = [
  match input with 
  | x::xs ->
    yield! [ for x' in xs do yield [x; x'] ]
    yield! combinations (x::acc) xs
  | [] -> ()
  yield! [ for x in input do yield [x] ]
]

let possiblePaths path = seq {
  let move current next = seq {
    for xs in combinations [] current do
    let newNext = List.sort (List.append xs next)
    let newPrev = List.sort (List.except xs current)
    if check newNext && check newPrev then
      yield newPrev, newNext
  }
  let (Node (n, downs, current, ups)) = path
  match downs with
  | (x::xs) ->
    yield! move current x |> Seq.map (fun (prev, next) -> Node (n - 1, xs, next, prev::ups))
  | [] -> ()
  match ups with
  | (x::xs) ->
    yield! move current x |> Seq.map (fun (prev, next) -> Node (n + 1, prev::downs, next, xs))
  | [] -> ()
}

let computeSteps root =
  let rec walk' s =
    let _, visited, pq, mEnd = s
    match PriorityQueue.tryPop pq with
    | Some ((_, step, path), pq') ->
      if pathCost path = 0
      then (step, visited, pq', Some path)
      else
        // Add to queue
        let v, q =
          let f acc x =
            let viz, que = acc
            if not <| Set.contains x viz
            then
              let cost = pathCost x
              let que' = PriorityQueue.insert (cost, step + 1, x) que
              (Set.add x viz, que')
            else acc
          possiblePaths path |> Seq.fold f (Set.add path visited, pq')
        walk' (step, v, q, None)
    | None -> s
  let start = mkPath root
  let cost = pathCost start
  let pq = PriorityQueue.insert (cost, 0, start) (PriorityQueue.empty false)
  let step, _, pq, endPath = walk' (0, Set.empty, pq, None)
  step, endPath


let testFloors = [
  []
  [ RTG "L" ]
  [ RTG "H" ]
  [ MEM "H"; MEM "L" ]
]

//The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator,
//  a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
//The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
//The third floor contains nothing relevant.
//The fourth floor contains nothing relevant.
let inputPuzzle = [
  []
  []
  [ MEM "polonium"; MEM "promethium" ]
  [ RTG "polonium"; RTG "thulium"; MEM "thulium"; RTG "promethium"; RTG "ruthenium"; MEM "ruthenium"; RTG "cobalt"; MEM "cobalt" ]
]

// vedran
//The first floor contains a promethium generator and a promethium-compatible microchip.
//The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
//The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
//The fourth floor contains nothing relevant.
let vedranPuzzle1 = [
  []
  [ MEM "cobalt"; MEM "curium"; MEM "ruthenium"; MEM "plutonium" ]
  [ RTG "cobalt"; RTG "curium"; RTG "ruthenium"; RTG "plutonium" ]
  [ RTG "promethium"; MEM "promethium" ]
]

//The first floor contains a promethium generator, a promethium-compatible microchip, an elerium generator, an elerium-compatible microchip, a dilithium generator, and a dilithium-compatible microchip.
//The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
//The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
//The fourth floor contains nothing relevant.
let vedranPuzzle2 = [
  []
  [ MEM "cobalt"; MEM "curium"; MEM "ruthenium"; MEM "plutonium" ]
  [ RTG "cobalt"; RTG "curium"; RTG "ruthenium"; RTG "plutonium" ]
  [ RTG "promethium"; MEM "promethium"; RTG "elerium"; MEM "elerium"; RTG "dilithium"; MEM "dilithium" ]
]

//An elerium generator.
//An elerium-compatible microchip.
//A dilithium generator.
//A dilithium-compatible microchip.
let inputPuzzlePart2 = [
  inputPuzzle.[0]
  inputPuzzle.[1]
  inputPuzzle.[2]
  inputPuzzle.[3]@[ RTG "elerium"; MEM "elerium"; RTG "dilithium"; MEM "dilithium" ]
]

computeSteps testFloors
computeSteps inputPuzzle
computeSteps inputPuzzlePart2
computeSteps vedranPuzzle1
computeSteps vedranPuzzle2
