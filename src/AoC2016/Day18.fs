(*
  AoC - Day 18

  http://adventofcode.com/2016/day/18
*)
module AoC.Day18

open Library

// PART 1

type Plate = Safe | Trap

let getNextPlate = function
  | Safe, Trap, Trap // Its center and right tiles are traps, but its left tile is not.
  | Trap, Trap, Safe // Its left and center tiles are traps, but its right tile is not.
  | Trap, Safe, Safe // Only its left tile is a trap.
  | Safe, Safe, Trap // Only its right tile is a trap.
      -> Trap
  | _ -> Safe

let parsePlate = function
  | '.' -> Safe
  | '^' -> Trap
  | s -> failwithf "What is that '%c'?" s

let parse (str : string) =
  str.ToCharArray()
  |> Seq.map parsePlate
  |> Seq.toList

let rec interleave3 = function
  | x::y::z::xs -> (x, y, z) :: interleave3 (y::z::xs)
  | [] | _::_   -> []

let genNextLine prevLine =
  // Add safe plate to each side
  interleave3 <| Safe::prevLine@[Safe]
  |> List.map getNextPlate 

let genNextNLines n prevLine =
  let f acc _ =
    let p,r = acc
    let nextLine = genNextLine p
    (nextLine, p :: r)
  Seq.fold f (prevLine, []) (seq { 0 .. n }) |> snd

let sumSafes xs =
  xs
  |> Seq.filter (function | Safe -> true | _ -> false)
  |> Seq.length

let safeCount rows input =
  genNextNLines (rows-1) (parse input)
  |> Seq.map sumSafes
  |> Seq.sum

// Example 1
let showPlate = function | Safe -> "." | Trap -> "^"

let print xs = 
  Seq.fold (+) "" (List.map showPlate xs)

let example1 = ".^^.^.^^^^"
let example1SafeCount = safeCount 10 example1
let exampleRes =
  genNextNLines 49 <| parse example1
  |> Seq.map print
  |> Seq.iter (printfn "%s")
