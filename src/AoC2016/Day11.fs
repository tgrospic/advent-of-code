(*
  AoC - Day 11

  http://adventofcode.com/2016/day/11
*)
module AoC.Day11

open Util
open FSharpx.Collections
open FSharpx.Functional
open FSharpx.Prelude

type Thing = RTG of string | MEM of string

type Path = Floor of int * (Thing list list) * Thing list * (Thing list list)

[<AutoOpen>]
module Parser =
  open FParsec
  open FParsecOp
  open FSharpx.Text

  let __ = spaces
  let pword1 = many1Satisfy System.Char.IsLetter
  let aan = pchar 'a' <* opt ^ pchar 'n'
  // Generator and microchip
  let pRTG = RTG <!> aan *> __ *> pword1 <* __ <* pstring "generator"
  let pMEM = MEM <!> aan *> __ *> pword1       <* pstring "-compatible microchip"
  // Sequence
  let pThing  = attempt pRTG <|> pMEM
  let pThings = sepBy pThing (pstring ", and " <|> pstring ", " <|> pstring " and ")
  // Line parser
  let pLinePrefix = pstring "The" *> __ *> pword1 *> __ *> pstring "floor contains" *> __
  let pLine       = pLinePrefix *> (pThings <* opt ^ pstring "nothing relevant") <* pchar '.'

  let private result = function
  | Success (x, _, _) -> x
  | Failure (s, _, _) -> failwith s

  let parser =
    Strings.toLines
    >> Seq.map ^ run pLine
    >> Seq.rev
    >> Seq.map result
    >> Seq.toList

// Check for microchip ashes
let check floor =
  let ty = function
    | MEM s when List.contains (RTG s) floor -> id
    | MEM _ -> Arrow.first ^ konst false
    | RTG _ -> Arrow.second ^ konst false
  List.fold (flip ty) (true, true) floor ||> (||)

// Calculate path cost to the end
let pathCost (Floor (_, downs, c, ups)) =
  List.append (List.rev downs) (c::ups)
  |> List.rev
  |> List.mapi ^ flip (List.length >> (*))
  |> List.sum

let rec combinations input = seq {
  match input with
  | x::xs ->
    yield! combinations xs
    yield! Seq.map (fun x' -> [x; x']) xs
    yield [x]
  | [] -> ()
}

// Move up and down, eliminate invalid paths
let possiblePaths path = seq {
  let move current next = seq {
    for xs in combinations current do
    let newNext = List.append xs next
    let newPrev = List.except xs current
    if check newNext && check newPrev then
      yield newPrev, newNext
  }
  let (Floor (n, downs, current, ups)) = path
  match downs with
  | (x::xs) ->
    yield! move current x |> Seq.map ^ fun (prev, next) -> Floor (n - 1, xs, next, prev::ups)
  | [] -> ()
  match ups with
  | (x::xs) ->
    yield! move current x |> Seq.map ^ fun (prev, next) -> Floor (n + 1, prev::downs, next, xs)
  | [] -> ()
}

// Example output: (0, [(1, 0); (2, 0); (3, 1); (3, 1)])
let hash1 (Floor (elevator, downs, c, ups)) =
  let ty = function | RTG _ -> 0 | MEM _ -> 1
  let normalize floorIdx = List.map (tuple2 floorIdx >> Arrow.second ty) >> List.sort
  List.append (List.rev downs) (c::ups)
  |> List.rev
  |> List.mapi normalize
  |> List.collect id
  |> tuple2 elevator

// Example output: (0, [(0, 0); (0, 6); (1, 0); (2, 0)])
let hash2 (Floor (elevator, downs, c, ups)) =
  let ty f = function | RTG _ -> Arrow.first f | MEM _ -> Arrow.second f
  let sum i = List.fold (flip ^ ty ((+)i)) (0, 0)
  List.append (List.rev downs) (c::ups)
  |> List.rev
  |> List.mapi sum
  |> List.sort
  |> tuple2 elevator

let computeSteps' hash root =
  let rec walk' (visited, pq) =
    match PriorityQueue.tryPop pq with
    | Some ((_, step, path), pq') ->
      if pathCost path = 0
      // Final answer found
      then Some (step, path)
      else
        // Add to queue
        let f acc x =
          let viz, que = acc
          if not <| Set.contains (hash x) viz
          then
            let cost = pathCost x + step
            let que' = PriorityQueue.insert (cost, step + 1, x) que
            Set.add (hash x) viz, que'
          else acc
        possiblePaths path
        |> Seq.fold f (Set.add (hash path) visited, pq')
        |> walk'
    | None -> None

  let cost = pathCost root
  let pq = PriorityQueue.insert (cost, 0, root) (PriorityQueue.empty false)
  Option.map fst ^ walk' (Set.empty, pq)

let mkPath s =
  match List.rev s with
  | x::xs -> Floor (0, [], x, xs)
  | []    -> Floor (0, [], [], [])

let computeSteps = parser >> mkPath >> computeSteps' hash2
